{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
module Passes.Inference where

import Annotation
import Errors
import Misc
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Maybe (fromJust)
import Data.List (nub)

import Types as T

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Parser.Lang as P

type Bindings = Map.Map Name Type
type Constraints = Map.Map TVar (Either [Trait] Type)
data Env = Env {
    bin_ops :: Bindings, -- binary defns
    un_ops :: Bindings, -- unary defns
    fn_defs :: Bindings, -- regular fn defns
    vars :: [Bindings], -- var defns
    tvars :: Constraints, -- type variable constraints
    count :: Int -- count to generate unique names
} deriving (Show, Eq)
newtype Inferred a = Inferred {
    unInferred :: ExceptT Error (State Env) a
} deriving (Functor, Applicative, Monad, MonadFix, MonadState Env)
throw :: Error -> Inferred a
throw = Inferred . throwE

-- | An infinite list of unique names
letters :: [String]
letters = [1..] >>= flip replicateM ['A'..'Z']
-- letters = map show [0..]

-- | Gets a fresh unique name (based on the state count)
fresh :: Inferred TVar
fresh = do
    c <- gets count
    modify $ \env -> env { count = c + 1 }
    return $ TV (letters !! c)

newScope, dropScope :: MonadState Env m => m ()
newScope = modify $ \env -> env { vars = Map.empty : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }

pushFnDef, pushBinOp, pushUnOp, pushVar :: MonadState Env m => Name -> Type -> m ()
pushFnDef name ty = modify $ \env -> env { fn_defs = Map.insert name ty $ fn_defs env }
pushBinOp name ty = modify $ \env -> env { bin_ops = Map.insert name ty $ bin_ops env }
pushUnOp name ty = modify $ \env -> env { un_ops = Map.insert name ty $ un_ops env }
pushVar name ty = modify $ \env ->
    if null $ vars env
        then env { vars = [Map.singleton name ty] }
        else env { vars = Map.insert name ty (head $ vars env) : tail (vars env) }
pushConstraint :: TVar -> Either [Trait] Type -> Inferred ()
pushConstraint var cs1 = do
    cs2 <- getConstraints var
    case cs2 of
        Just cs -> do
            ret <- resolveConstraint cs1 cs
            modify $ \env -> env { tvars = Map.insert var ret $ tvars env }
        Nothing -> modify $ \env -> env { tvars = Map.insert var cs1 $ tvars env }

getFnDef, getBinOp, getUnOp, getVar :: MonadState Env m => Name -> m (Maybe Type)
getFnDef name = gets $ \env -> env |> fn_defs |> Map.lookup name
getBinOp name = gets $ \env -> env |> bin_ops |> Map.lookup name
getUnOp name = gets $ \env -> env |> un_ops |> Map.lookup name
getVar name = gets $ \env -> env |> vars |> map (Map.lookup name) |> foldl (<|>) Nothing
getConstraints :: MonadState Env m => TVar -> m (Maybe (Either [Trait] Type))
getConstraints tv = gets $ \Env{ tvars } -> tvars |> Map.lookup tv

class Infer a where
    infer :: a b -> Inferred (a Type)

instance Infer P.Arg where
    infer arg@(P.Arg range name ty) = do
        tv <- fresh
        pushVar name (TVar tv)
        pushConstraint tv (Left [])
        return $ P.Arg (TVar tv) name ty

instance Infer P.Stmt where
    infer (P.Defn range defnTy name args user_ret body) = mdo
        newScope
        annotated_args <- mapM infer args
        let user_args = map (\(P.Arg _ _ ty) -> ty) annotated_args
        let user_prototype = TFun Map.empty user_args user_ret
        let inferred_prototype = TFun Map.empty inferred_args inferred_ret
        let (getf, pushf) = case defnTy of
             P.Function -> (getFnDef, pushFnDef)
             P.Unary -> (getUnOp, pushUnOp)
             P.Binary -> (getBinOp, pushBinOp)
        maybe_fn <- getf name
        case maybe_fn of
            Just ty2 -> throw $ MultipleDefnError name [ty2, inferred_prototype]
            Nothing -> pushf name inferred_prototype
        annotated_body <- infer body
        inferred_args <- fmap (map fromJust) (args |> mapM (\(P.Arg _ name _) -> getVar name))
        let inferred_ret = P.getExprAnn annotated_body
        dropScope
        forM (zip user_args inferred_args) $ \(u, TVar i) -> pushConstraint i (Right u)
        return $ P.Defn inferred_prototype defnTy name annotated_args user_ret annotated_body
    infer (P.Expr range expr) = do
        annotated_expr <- infer expr
        return $ P.Expr (P.getExprAnn annotated_expr) annotated_expr
    infer (P.Extern range name args ret_ty) = do
        newScope
        annotated_args <- mapM infer args
        let tys = map P.getArgAnn annotated_args
        let ty = TFun Map.empty tys ret_ty
        maybe_fn <- getFnDef name
        case maybe_fn of
            Just ty2 -> throw $ MultipleDefnError name [ty2, ty]
            Nothing -> pushFnDef name ty
        dropScope
        return $ P.Extern ty name annotated_args ret_ty

instance Infer P.Expr where
    infer (P.For range init cond oper body) = do
        newScope
        tys <- mapM infer [init, cond, oper]
        annotated_body <- infer body
        let ty = P.getExprAnn annotated_body
        dropScope
        return $ P.For ty (tys !! 0) (tys !! 1) (tys !! 2) annotated_body
    infer (P.If range cond then_body else_body) = do
        newScope
        tv <- fresh
        annotated_cond <- infer cond
        annotated_then <- infer then_body
        let then_ty = P.getExprAnn annotated_then
        pushConstraint tv (Right then_ty)
        case else_body of
            Nothing -> do
                dropScope
                return $ P.If then_ty annotated_cond annotated_then Nothing
            Just block -> do
                annotated_else <- infer block
                let else_ty = P.getExprAnn annotated_else
                pushConstraint tv (Right else_ty)
                dropScope
                return $ P.If (TVar tv) annotated_cond annotated_then (Just annotated_else)
    infer (P.While range cond body) = do
        newScope
        annotated_cond <- infer cond
        annotated_body <- infer body
        let body_ty = P.getExprAnn annotated_body
        dropScope
        return $ P.While body_ty annotated_cond annotated_body
    infer (P.Call range (Ann _ name) args) = do
        fun_ty <- do
            found <- gets $ \Env { fn_defs } ->
                Map.lookup name fn_defs
            maybe
                (throw $ NotInScopeError name)
                return
                found
        annotated_args <- mapM infer args
        let args_tys = map P.getExprAnn annotated_args
        ty <- apply fun_ty args_tys
        return $ P.Call ty (Ann fun_ty name) annotated_args
    infer (P.Bin range (Ann _ "=") lhs rhs) =
        case lhs of
            P.Ident _ name -> do
                tv <- fresh
                annotated_rhs <- infer rhs
                let ty = P.getExprAnn annotated_rhs
                pushVar name (TVar tv)
                pushConstraint tv (Right ty)
                return $ P.Bin ty (Ann (TFun Map.empty [ty] ty) "=") (P.Ident ty name) annotated_rhs
            _ -> throw AssignError
    infer (P.Bin range (Ann _ name) lhs rhs) = do
        fun_ty <- do
            found <- gets $ \Env { bin_ops } ->
                Map.lookup name bin_ops
            maybe
                (throw $ NotInScopeError name)
                return
                found
        annotated_args <- mapM infer [lhs, rhs]
        let args_tys = map P.getExprAnn annotated_args
        ty <- apply fun_ty args_tys
        return $ P.Bin ty (Ann fun_ty name) (annotated_args !! 0) (annotated_args !! 1)
    infer (P.Un range (Ann _ name) rhs) = do
        fun_ty <- do
            found <- gets $ \Env { un_ops } ->
                Map.lookup name un_ops
            maybe
                (throw $ NotInScopeError name)
                return
                found
        annotated_args <- mapM infer [rhs]
        let args_tys = map P.getExprAnn annotated_args
        ty <- apply fun_ty args_tys
        return $ P.Un ty (Ann fun_ty name) (annotated_args !! 0)
    infer (P.Ident range ident) = do
        found <- gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (Map.lookup ident) (vars <> [fn_defs, un_ops, bin_ops])
        maybe
            (throw $ NotInScopeError ident)
            (\ty -> return $ P.Ident ty ident)
            found
    infer (P.Lit range lit@(P.IntLiteral _)) = do
        tv <- fresh
        pushConstraint tv (Left [Trait "Num"])
        return $ P.Lit (TVar tv) lit
    infer (P.Lit range lit@(P.DoubleLiteral _)) = do
        tv <- fresh
        pushConstraint tv (Left [Trait "Fractional"])
        return $ P.Lit (TVar tv) lit
    infer (P.Lit range lit@(P.BooleanLiteral _)) =
        return $ P.Lit T.bool lit
    infer (P.Lit range lit@P.VoidLiteral) =
        return $ P.Lit T.void lit

implementsTraits :: [Trait] -> TCon -> Inferred ()
implementsTraits traits ty = forM_ traits $ \trait ->
    case Map.lookup trait traitsTable of
        Nothing -> throw $ TraitNotInScopeError trait
        Just types ->
            if notElem ty types
                then throw $ NotImplTraitError (TCon ty) trait
                else return ()

-- TODO: Better document this function (or I won't be able to ever read it again).
-- type Apply = ExceptT Error (State (Map.Map TVar (Either [Trait] TCon)))
-- apply' :: (Type, Type) -> Apply Type
-- apply' (expected@(TCon _), got@(TCon _)) =
--     if got == expected
--         then return got
--         else throw $ TypeErr $ TypeError { expected, got }
-- apply' (TVar var@(TV name), got@(TCon cty)) = do
--     maybe_ty <- gets $ Map.lookup var
--     maybe
--         (throw $ NotInScopeError name)
--         ret
--         maybe_ty
--     where
--         ret :: Either [Trait] TCon -> Apply Type
--         ret (Left traits) = do
--             implementsTraits traits cty
--             modify $ Map.insert var $ Right cty
--             return got
--         ret (Right expected) =
--             if TCon expected == got
--                 then return got
--                 else throw $ TypeErr $ TypeError { expected = TCon expected, got }
-- apply' (expected@(TCon cty), got@(TVar var)) = do
--     maybe_ret <- gets (\env -> Map.lookup var $ tvars env)
--     case maybe_ret of
--         Left traits -> do
--             implementsTraits traits cty
--             modify $ \env -> env { tvars = Map.insert var (Right expected) (tvars env) }
--             return expected
--         Right ty ->
--             if expected == ty
--                 then return expected
--                 else throw $ TypeErr $ TypeError { expected, got = ty }
-- apply' (expected@(TVar _), got@(TVar _)) = error "Attempted (TVar <-> TVar)"
-- apply' _ = error "Unexpected type variable, really should never happen"

-- Applies an argument list to a function, returning its result type if all types matches.
-- Supports parametric polymorphism.
-- TODO: Better document this function (or I won't be able to ever read it again).
apply :: Type -> [Type] -> Inferred Type
apply (TFun constraints t1s t2) t3s | length t1s == length t3s = do
    freshVars <- forM (constraints |> Map.toList) $ \(name, cs) -> do
         tv <- fresh
         pushConstraint tv (Left cs)
         return (TVar tv)
    let ftvMap = zip (constraints |> Map.keys) freshVars
    s_t1s <- forM t1s $ \ty ->
        case ty of
            TVar v -> case lookup v ftvMap of
                Just f -> return f
                Nothing -> return ty
            _ -> return ty
    s_t2 <- case t2 of
        TVar v -> case lookup v ftvMap of
            Just f -> return f
            Nothing -> return t2
        _ -> return t2
    forM_ (zip t3s s_t1s) $ \(u, TVar i) -> pushConstraint i (Right u)
    return s_t2
apply (TFun _ t1s _) t3s =
    throw $ ArgCountError (length t1s) (length t3s)

freeTypeVars :: Type -> Set.Set TVar
freeTypeVars (TVar tv) = Set.singleton tv
freeTypeVars (TCon _) = Set.empty
freeTypeVars (TFun cs args ret) =
    let fvars = ((ret : args) |> filter isTVar |> map (\(TVar tv) -> tv) |> Set.fromList)
    in Set.difference fvars (cs |> Map.keys |> Set.fromList)

substitute :: Either [Trait] Type -> Inferred (Either [Trait] Type)
substitute (Left traits) = return (Left traits)
substitute (Right (TVar tv)) = do
    maybe_cs <- getConstraints tv
    case maybe_cs of
        Just cs -> substitute cs
        Nothing -> return (Right $ TVar tv)
substitute (Right ty) = return (Right ty)

resolveConstraint :: Either [Trait] Type -> Either [Trait] Type -> Inferred (Either [Trait] Type)
resolveConstraint acc elem = do
    s1 <- substitute acc
    s2 <- substitute elem
    case (s1, s2) of
        (Left traits1, Left traits2) ->
            return (Left $ nub $ traits1 <> traits2)
        (Left traits1, Right (TCon t2)) -> do
            implementsTraits traits1 t2
            return (Right $ TCon t2)
        (Right (TVar t1), Left traits2) ->
            return (Left traits2)
        (Right (TCon t1), Left traits2) -> do
            implementsTraits traits2 t1
            return (Right $ TCon t1)
        (Right t1, Right t2) ->
            if t1 == t2
                then return (Right t1)
                else throw $ TypeError t1 t2
        err -> do
            env <- get
            error $ "Not unifiable: " <> show err <> " [Env: " <> show env <> "]"

-- Avoid infinite loops ? (freeTypeVars function ?)
unify :: Type -> Type -> Inferred Type
unify t1@(TCon   _) t2@(TCon   _) | t1 == t2 = return t1
unify t1@(TVar tv1) t2@(TCon tc2) = do
    ret <- getConstraints tv1
    case ret of
        Just ty -> case ty of
            Left traits -> do
                implementsTraits traits tc2
                return t2
            Right tv -> unify tv t2
        Nothing -> do
            env <- get
            error $ "Unexpected unresolved constraint: " <> show (t1, t2) <> "\nEnv: " <> show env
unify t1@(TCon tc1) t2@(TVar tv2) = do
    ret <- getConstraints tv2
    case ret of
        Just ty -> case ty of
            Left traits -> do
                implementsTraits traits tc1
                return t1
            Right tv -> unify tv t1
        Nothing -> do
            env <- get
            error $ "Unexpected unresolved constraint: " <> show (t1, t2) <> "\nEnv: " <> show env
-- unify t1@(TVar tv1) t2@(TVar tv2) = do
--     pushConstraint tv1 (Right t2)
--     return t2
unify t1 t2 = throw $ TypeError t1 t2

inferAST :: P.AST a -> (Either Error (P.AST Type), Env)
inferAST stmts =
    stmts |> mapM infer
          |> unInferred
          |> runExceptT
          |> flip runState defaultEnv

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = T.builtinBinaryOps,
    un_ops = T.builtinUnaryOps,
    fn_defs = T.builtinFunctions,
    vars = [],
    tvars = Map.empty,
    count = 0
}
