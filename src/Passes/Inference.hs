{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Passes.Inference where

import Annotation
import Errors
import Misc
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe (fromJust)
import Data.List (nub, partition)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

import Types as T

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Parser.Lang as P
import qualified Parser.Lib as L

newtype TypeEnv = TypeEnv (Map.Map Name Scheme) deriving (Show, Eq)
instance Substitutable TypeEnv where
    applySubst s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)
    freeTypeVars (TypeEnv env) = env |> Map.elems
                                     |> map freeTypeVars
                                     |> Set.unions
extendEnv :: Name -> Scheme -> TypeEnv -> TypeEnv
extendEnv name ty (TypeEnv env) = TypeEnv (Map.insert name ty env)
lookupScheme :: Name -> TypeEnv -> Maybe Scheme
lookupScheme name (TypeEnv env) = Map.lookup name env
lookupEnv :: Name -> TypeEnv -> Infer Type
lookupEnv name (TypeEnv env) =
    case Map.lookup name env of
        Just scheme -> instantiate scheme
        Nothing -> throwError $ NotInScopeError name

data Env = Env {
    bin_ops :: TypeEnv, -- binary defns
    un_ops :: TypeEnv,  -- unary defns
    fn_defs :: TypeEnv, -- regular fn defns
    vars :: [TypeEnv],  -- var defns
    tvars :: [Constraint], -- type variable constraints
    count :: Int  -- count to generate unique names
} deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = TypeEnv (Map.fromList T.builtinBinaryOps),
    un_ops = TypeEnv (Map.fromList T.builtinUnaryOps),
    fn_defs = TypeEnv (Map.fromList T.builtinFunctions),
    vars = [TypeEnv Map.empty],
    tvars = [],
    count = 0
}

newtype Infer a = Infer {
    runInfer :: ExceptT Error (State Env) a
} deriving (Functor, Applicative, Monad, MonadFix, MonadState Env, MonadError Error)

newScope, dropScope :: MonadState Env m => m ()
newScope = modify $ \env -> env { vars = TypeEnv Map.empty : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }
withScope :: MonadState Env m => m a -> m a
withScope action = do
    newScope
    ret <- action
    dropScope
    return ret

pushConstraint :: MonadState Env m => Constraint -> m ()
pushConstraint constraint = modify $ \env -> env { tvars = constraint : tvars env }
pushFnDef, pushBinOp, pushUnOp, pushVar :: Name -> Scheme -> Infer ()
pushFnDef name ty = modify $ \env -> env { fn_defs = extendEnv name ty $ fn_defs env }
pushBinOp name ty = modify $ \env -> env { bin_ops = extendEnv name ty $ bin_ops env }
pushUnOp name ty = modify $ \env -> env { un_ops = extendEnv name ty $ un_ops env }
pushVar name ty = modify $ \env ->
    case vars env of
        [] -> env { vars = [TypeEnv (Map.singleton name ty)] }
        (x:xs) -> env { vars = extendEnv name ty x : xs }

getFnDef, getBinOp, getUnOp :: Name -> Infer (Maybe Scheme)
getFnDef name = gets $ \env -> env |> fn_defs |> lookupScheme name
getBinOp name = gets $ \env -> env |> bin_ops |> lookupScheme name
getUnOp name = gets $ \env -> env |> un_ops |> lookupScheme name
getVar :: Name -> Infer Type
getVar name = do
    ret <- gets $ \env -> env |> vars |> map (lookupScheme name) |> foldl (<|>) Nothing
    case ret of
        Just scheme -> instantiate scheme
        Nothing -> throwError $ NotInScopeError name

-- | An infinite list of unique names
letters :: [String]
letters = [1..] >>= flip replicateM ['A'..'Z']

-- | Gets a fresh unique name (based on the state count)
fresh :: MonadState Env m => m Type
fresh = do
    idx <- gets count
    modify $ \env -> env { count = idx + 1 }
    return $ TVar $ TV (letters !! idx)

inferArg :: P.LocatedArg -> Infer P.TypedArg
inferArg arg@(Ann range (P.Arg name ty)) = do
    pushVar name ([] :=> ty)
    return $ Ann (range, ty)
           $ P.Arg name ty

inferStmt :: P.LocatedStmt -> Infer P.TypedStmt
inferStmt = \case
    Ann range (P.Defn defn_ty name args user_ret body) ->
        withScope $ do
            tv <- fresh
            inferred_args <- mapM inferArg args
            let user_args = map (\(Ann _ (P.Arg _ ty)) -> ty) inferred_args
            let user_prototype = user_args :-> user_ret
            let (getf, pushf) = case defn_ty of
                 P.Function -> (getFnDef, pushFnDef)
                 P.Unary _ -> (getUnOp, pushUnOp)
                 P.Binary _ -> (getBinOp, pushBinOp)
            maybe_fn <- getf name
            case maybe_fn of
                Just ty2 -> throwError $ MultipleDefnError name [ty2, [] :=> user_prototype]
                Nothing -> pushf name ([] :=> user_prototype)
            inferred_body <- inferExpr body
            pushConstraint $ Matches tv $ snd $ annotation inferred_body
            pushConstraint $ Matches tv user_ret
            -- forM (zip user_args inferred_args) $ \(u, TVar i) -> pushConstraint i (Right u)
            -- final_ret <- case inferred_ret_ty of
            --     TVar tv -> pushConstraint tv (Right user_ret) >> return inferred_ret_ty
            --     ty -> unify ty user_ret
            return $ Ann (range, user_args :-> tv)
                   $ P.Defn defn_ty name inferred_args user_ret inferred_body
    Ann range (P.Expr expr) -> do
        tv <- fresh
        inferred_expr <- inferExpr expr
        pushConstraint $ Matches tv $ snd $ annotation inferred_expr
        return $ Ann (range, tv)
               $ P.Expr inferred_expr
    Ann range (P.Extern name args ret_ty) ->
        withScope $ do
            inferred_args <- mapM inferArg args
            let args = map (snd . annotation) inferred_args
            let prototype = args :-> ret_ty
            maybe_fn <- getFnDef name
            case maybe_fn of
                Just ty2 -> throwError $ MultipleDefnError name [ty2, [] :=> prototype]
                Nothing -> pushFnDef name ([] :=> prototype)
            return $ Ann (range, prototype)
                   $ P.Extern name inferred_args ret_ty

inferExpr :: P.LocatedExpr -> Infer P.TypedExpr
inferExpr = \case
    Ann range (P.For init cond oper body) ->
        withScope $ do
            tv <- fresh
            inferred_init <- inferExpr init
            inferred_cond <- inferExpr cond
            inferred_oper <- inferExpr oper
            inferred_body <- inferExpr body
            pushConstraint $ Matches tv $ snd $ annotation inferred_body
            return $ Ann (range, tv)
                   $ P.For inferred_init inferred_cond inferred_oper inferred_body
    Ann range (P.If cond then_body else_body) ->
        withScope $ do
            tv <- fresh
            inferred_cond <- inferExpr cond
            inferred_then <- inferExpr then_body
            pushConstraint $ Matches tv $ snd $ annotation inferred_then
            case else_body of
                Nothing -> return $ Ann (range, tv)
                                  $ P.If inferred_cond inferred_then Nothing
                Just block -> do
                    inferred_else <- inferExpr block
                    pushConstraint $ Matches tv $ snd $ annotation inferred_else
                    return $ Ann (range, tv)
                           $ P.If inferred_cond inferred_then (Just inferred_else)
    Ann range (P.While cond body) ->
        withScope $ do
            tv <- fresh
            inferred_cond <- inferExpr cond
            inferred_body <- inferExpr body
            pushConstraint $ Matches tv $ snd $ annotation inferred_body
            return $ Ann (range, tv)
                   $ P.While inferred_cond inferred_body
    Ann range (P.Call (Ann range2 name) args) -> do
        prototype <- join $ gets $ \env -> env |> fn_defs |> lookupEnv name
        inferred_args <- mapM inferExpr args
        let args_tys = map (snd . annotation) inferred_args
        ret_ty <- apply prototype args_tys
        return $ Ann (range, ret_ty)
               $ P.Call (Ann (range2, prototype) name) inferred_args
    Ann range (P.Bin (Ann range2 "=") lhs rhs) ->
        case lhs of
            Ann range3 (P.Ident name) -> do
                tv <- fresh
                inferred_rhs <- inferExpr rhs
                pushConstraint $ Matches tv $ snd $ annotation inferred_rhs
                pushVar name ([] :=> tv)
                return $ Ann (range, tv)
                       $ P.Bin (Ann (range2, [tv] :-> tv) "=") (Ann (range3, tv) (P.Ident name)) inferred_rhs
            _ -> throwError AssignError
    Ann range (P.Bin (Ann range2 name) lhs rhs) -> do
        prototype <- join $ gets $ \env -> env |> bin_ops |> lookupEnv name
        inferred_lhs <- inferExpr lhs
        inferred_rhs <- inferExpr rhs
        let args_tys = map (snd . annotation) [inferred_lhs, inferred_rhs]
        ret_ty <- apply prototype args_tys
        return $ Ann (range, ret_ty)
               $ P.Bin (Ann (range2, prototype) name) inferred_lhs inferred_rhs
    Ann range (P.Un (Ann range2 name) rhs) -> do
        prototype <- join $ gets $ \env -> env |> un_ops |> lookupEnv name
        inferred_rhs <- inferExpr rhs
        ret_ty <- apply prototype [snd $ annotation inferred_rhs]
        return $ Ann (range, ret_ty)
               $ P.Un (Ann (range2, prototype) name) inferred_rhs
    Ann range (P.Ident ident) -> do
        found <- gets $ \env ->
            (vars env <> [fn_defs env, un_ops env, bin_ops env]) |> fmap (lookupScheme ident)
                                                                 |> foldl (<|>) Nothing
        case found of
            Nothing -> throwError $ NotInScopeError ident
            Just scheme -> do
                ty <- instantiate scheme
                return $ Ann (range, ty)
                       $ P.Ident ident
    Ann range (P.Lit lit@(P.IntLiteral _)) -> do
        tv <- fresh
        pushConstraint $ Implements tv ["Num"]
        return $ Ann (range, tv)
               $ P.Lit lit
    Ann range (P.Lit lit@(P.DoubleLiteral _)) -> do
        tv <- fresh
        pushConstraint $ Implements tv ["Fractional"]
        return $ Ann (range, tv)
               $ P.Lit lit
    Ann range (P.Lit lit@(P.BooleanLiteral _)) ->
        return $ Ann (range, T.bool)
               $ P.Lit lit
    Ann range (P.Lit lit@P.VoidLiteral) ->
        return $ Ann (range, T.void)
               $ P.Lit lit

implementsTraits :: MonadError Error m => [Trait] -> TCon -> m ()
implementsTraits traits ty = forM_ traits $ \trait ->
    case Map.lookup trait traitsTable of
        Nothing -> throwError $ TraitNotInScopeError trait
        Just (types, _) ->
            if notElem ty types
                then throwError $ NotImplTraitError (TCon ty) trait
                else return ()

defaultTraitType :: MonadError Error m => Trait -> m Type
defaultTraitType trait =
    case Map.lookup trait traitsTable of
        Nothing -> throwError $ TraitNotInScopeError trait
        Just (_, def) -> return (TCon def)

apply :: Type -> [Type] -> Infer Type
apply (t1s :-> t2) t3s | length t1s == length t3s = do
    forM_ (zip t1s t3s) $ \case
        (TVar tv, t3) -> pushConstraint $ Matches (TVar tv) t3
        (t1, TVar tv) -> pushConstraint $ Matches (TVar tv) t1
        (t1, t3) | t1 == t3 -> return ()
        (t1, t3) -> throwError $ TypeError t1 t3
    return t2
apply (t1s :-> _) t3s =
    throwError $ ArgCountError (length t1s) (length t3s)
apply _ _ = error "Non-function call application"

(<<>>) :: TVar -> Type -> Solve Subst
name <<>> (TVar v) | boundToSelf = return mempty
    where boundToSelf = name == v
name <<>> ty | name >|< ty = throwError (CantConstructInfiniteType name ty)
    where n >|< t = Set.member n (freeTypeVars t)
name <<>> ty = return (Subst $ Map.singleton (TVar name) ty)

generalize :: [TVar] -> Type -> Scheme
generalize env ty = (cs |> Set.toList |> map (, [])) :=> ty
    where cs = Set.difference (T.freeTypeVars ty) (Set.fromList env)

instantiate :: Scheme -> Infer Type
instantiate (cs :=> ty) = do
        subst <- substituteAllWithFresh cs
        return (applySubst subst ty)
    where
        substituteAllWithFresh :: [(TVar, [Trait])] -> Infer Subst
        substituteAllWithFresh xs = do
            let action (tvar, traits) = do
                 tv <- fresh
                 pushConstraint $ Implements tv traits
                 return (TVar tvar, tv)
            freshSubstActions <- mapM action xs
            let freshSubsts = Map.fromList freshSubstActions
            return (Subst freshSubsts)

infer :: P.LocatedAST -> Either Error P.TypedAST
infer stmts = do
    let (maybe_ast, env) = stmts |> mapM inferStmt
                                 |> runInfer
                                 |> runExceptT
                                 |> flip runState defaultEnv
    let (impls, matches) = env |> tvars |> reverse |> partition isImplConstraint
    let merged = foldl (\acc (Implements var traits) -> Map.insertWith (<>) var traits acc) Map.empty impls
    let final_impls = merged |> Map.toList |> map (\(var, traits) -> Implements var traits)
    ast <- maybe_ast
    solutions <- runSolve (matches <> reverse final_impls)
    return (ast |> mapM substStmt |> flip runReader solutions)

inferAST :: P.LocatedAST -> (Either Error P.TypedAST, Env)
inferAST stmts =
    stmts |> mapM inferStmt
          |> runInfer
          |> runExceptT
          |> flip runState defaultEnv

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v <<>> t
unifies t (TVar v) = v <<>> t
unifies (t1s :-> t2) (t3s :-> t4) = unifyMany (zip t1s t3s <> [(t2, t4)])
unifies t1 t2 = throwError $ TypeError t1 t2

unifyMany :: [(Type, Type)] -> Solve Subst
unifyMany [] = return mempty
unifyMany ((t1, t2) : ts) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (applySubst su1 ts)
     return (su2 <> su1)

remains :: Type -> [Constraint] -> Bool
remains t1 = \case
    (Matches t2 _:_) | t1 == t2 -> True
    (Matches _ t2:_) | t1 == t2 -> True
    (_:cs) -> remains t1 cs
    [] -> False

solveConstraints :: Solve Subst
solveConstraints = do
    (su, constraints) <- get
    case constraints of
        [] -> return su
        (Matches var ty: cs) -> do
            su1 <- unifies var ty
            put (su1 <> applySubst su1 su, applySubst su1 cs)
            solveConstraints
        (Implements var traits: cs) ->
            case var of
                TVar _ -> case getSubst var su of
                    Just (TCon tc) -> do
                        implementsTraits traits tc
                        put (su, cs)
                        solveConstraints
                    Just (TVar _) | remains var cs -> do
                        put (su, cs <> [Implements var traits])
                        solveConstraints
                    Just (TVar tv) -> do -- error "Unresolved type variable"
                        let action trait = do
                             def <- defaultTraitType trait
                             let TCon tc = def
                             implementsTraits traits tc
                             return def
                        ty <- traits |> map action |> foldl1 (<|>)
                        let su1 = Subst (Map.singleton var ty)
                        seq (unsafePerformIO $ putStrLn $ show su1) (return ())
                        seq (unsafePerformIO $ putStrLn $ show traits) (return ())
                        seq (unsafePerformIO $ putStrLn $ show cs) (return ())
                        put (su1 <> applySubst su1 su, applySubst su1 cs)
                        solveConstraints
                    Nothing | remains var cs -> do
                        put (su, cs <> [Implements var traits])
                        solveConstraints
                    Nothing -> do
                        let action trait = do
                             def <- defaultTraitType trait
                             let TCon tc = def
                             implementsTraits traits tc
                             let su1 = Subst (Map.singleton var def)
                             put (su1 <> applySubst su1 su, applySubst su1 cs)
                             solveConstraints
                             return def
                        ty <- traits |> map action |> foldl1 (<|>)
                        let su1 = Subst (Map.singleton var ty)
                        seq (unsafePerformIO $ putStrLn $ show su1) (return ())
                        seq (unsafePerformIO $ putStrLn $ show traits) (return ())
                        seq (unsafePerformIO $ putStrLn $ show cs) (return ())
                        put (su1 <> applySubst su1 su, applySubst su1 cs)
                        solveConstraints
                TCon tc -> do
                    implementsTraits traits tc
                    put (su, cs)
                    solveConstraints
                _ -> error "Funtion type asked to implements traits"

type Unifier = (Subst, [Constraint])
type Solve a = ExceptT Error (State Unifier) a

runSolve :: [Constraint] -> Either Error Subst
runSolve cs = evalState (runExceptT solveConstraints) (mempty, cs)

type Substitute a = Reader Subst a

substStmt :: P.TypedStmt -> Substitute P.TypedStmt
substStmt = \case
    Ann (range, ty) (P.Defn defn_ty name args user_ret body) -> do
        s <- ask
        substituted_body <- substExpr body
        return $ Ann (range, applySubst s ty)
               $ P.Defn defn_ty name args user_ret substituted_body
    Ann (range, ty) (P.Expr expr) -> do
        s <- ask
        substituted_expr <- substExpr expr
        return $ Ann (range, applySubst s ty)
               $ P.Expr substituted_expr
    Ann (range, ty) (P.Extern name args user_ret) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Extern name args user_ret

substExpr :: P.TypedExpr -> Substitute P.TypedExpr
substExpr = \case
    Ann (range, ty) (P.For init cond oper body) -> do
        s <- ask
        substituted_init <- substExpr init
        substituted_cond <- substExpr cond
        substituted_oper <- substExpr oper
        substituted_body <- substExpr body
        return $ Ann (range, applySubst s ty)
               $ P.For substituted_init substituted_cond substituted_oper substituted_body
    Ann (range, ty) (P.If cond then_body else_body) -> do
        s <- ask
        substituted_cond <- substExpr cond
        substituted_then <- substExpr then_body
        case else_body of
            Nothing -> return $ Ann (range, applySubst s ty)
                              $ P.If substituted_cond substituted_then Nothing
            Just block -> do
                substituted_else <- substExpr block
                return $ Ann (range, applySubst s ty)
                       $ P.If substituted_cond substituted_then (Just substituted_else)
    Ann (range, ty) (P.While cond body) -> do
        s <- ask
        substituted_cond <- substExpr cond
        substituted_body <- substExpr body
        return $ Ann (range, applySubst s ty)
               $ P.While substituted_cond substituted_body
    Ann (range, ty) (P.Call (Ann (range2, ty2) name) args) -> do
        s <- ask
        substituted_args <- mapM substExpr args
        return $ Ann (range, applySubst s ty)
               $ P.Call (Ann (range2, applySubst s ty2) name) substituted_args
    Ann (range, ty) (P.Bin (Ann (range2, ty2) name) lhs rhs) -> do
        s <- ask
        substituted_lhs <- substExpr lhs
        substituted_rhs <- substExpr rhs
        return $ Ann (range, applySubst s ty)
               $ P.Bin (Ann (range2, applySubst s ty2) name) substituted_lhs substituted_rhs
    Ann (range, ty) (P.Un (Ann (range2, ty2) name) rhs) -> do
        s <- ask
        substituted_rhs <- substExpr rhs
        return $ Ann (range, applySubst s ty)
               $ P.Un (Ann (range2, applySubst s ty2) name) substituted_rhs
    Ann (range, ty) (P.Ident ident) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Ident ident
    Ann (range, ty) (P.Lit lit@(P.IntLiteral _)) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Lit lit
    Ann (range, ty) (P.Lit lit@(P.DoubleLiteral _)) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Lit lit
    Ann (range, ty) (P.Lit lit@(P.BooleanLiteral _)) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Lit lit
    Ann (range, ty) (P.Lit lit@P.VoidLiteral) -> do
        s <- ask
        return $ Ann (range, applySubst s ty)
               $ P.Lit lit
