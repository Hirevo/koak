{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Passes.Annotation where

import Annotation
import Errors
import Misc
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except

import Types as T

import qualified Data.Map as Map
import qualified Parser.Lang as P

type Scope = Map.Map Name Type
type Constraints = Map.Map TVar (Either [Trait] Type)
data Env = Env {
    bin_ops :: Scope,
    un_ops :: Scope,
    fn_defs :: Scope,
    vars :: [Scope],
    tvars :: Map.Map TVar (Either [Trait] Type),
    count :: Int
} deriving (Show, Eq)
type Annotated = ExceptT Error (State Env)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']
-- letters = map show [0..]

fresh :: Annotated TVar
fresh = do
    c <- gets count
    modify $ \env -> env { count = c + 1 }
    return $ TV (letters !! c)

newScope, dropScope :: State Env ()
newScope = modify $ \env -> env { vars = Map.empty : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }

pushFnDef, pushBinOp, pushUnOp, pushVar :: Name -> Type -> State Env ()
pushFnDef name ty = modify $ \env -> env { fn_defs = Map.insert name ty $ fn_defs env }
pushBinOp name ty = modify $ \env -> env { bin_ops = Map.insert name ty $ bin_ops env }
pushUnOp name ty = modify $ \env -> env { un_ops = Map.insert name ty $ un_ops env }
pushVar name ty = modify $ \env ->
    if null $ vars env
        then env { vars = [Map.singleton name ty] }
        else env { vars = Map.insert name ty (head $ vars env) : tail (vars env) }

getFnDef, getBinOp, getUnOp, getVar :: Name -> State Env (Maybe Type)
getFnDef name = gets $ \env -> env |> fn_defs |> Map.lookup name
getBinOp name = gets $ \env -> env |> bin_ops |> Map.lookup name
getUnOp name = gets $ \env -> env |> un_ops |> Map.lookup name
getVar name = gets $ \env -> env |> vars |> map (Map.lookup name) |> foldl1 (<|>)

class Annotate1 a where
    annotate1 :: a b -> Annotated (a Type)

instance Annotate1 P.Arg where
    annotate1 arg@(P.Arg range name ty) = do
        lift $ pushVar name ty
        return $ P.Arg ty name ty

instance Annotate1 P.Stmt where
    annotate1 (P.Defn range defnTy name args ret_ty body) = do
        lift newScope
        annotated_args <- mapM annotate1 args
        let tys = map P.getArgAnn annotated_args
        let ty = TFun Map.empty tys ret_ty
        let (getf, pushf) = case defnTy of
             P.Function -> (getFnDef, pushFnDef)
             P.Unary -> (getUnOp, pushUnOp)
             P.Binary -> (getBinOp, pushBinOp)
        maybe_fn <- lift $ getf name
        case maybe_fn of
            Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                name, definitions = [ty2, ty]
            }
            Nothing -> lift $ pushf name ty
        annotated_body <- annotate1 body
        let inferred = P.getExprAnn annotated_body
        lift dropScope
        if inferred == ret_ty
            then return $ P.Defn ty defnTy name annotated_args ret_ty annotated_body
            else throwE $ TypeErr $ TypeError {
                expected = ret_ty,
                got = inferred
            }
    annotate1 (P.Expr range expr) = do
        annotated_expr <- annotate1 expr
        return $ P.Expr (P.getExprAnn annotated_expr) annotated_expr
    annotate1 (P.Extern range name args ret_ty) = do
        lift newScope
        annotated_args <- mapM annotate1 args
        let tys = map P.getArgAnn annotated_args
        let ty = TFun Map.empty tys ret_ty
        maybe_fn <- lift $ getFnDef name
        case maybe_fn of
            Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                name, definitions = [ty2, ty]
            }
            Nothing -> lift $ pushFnDef name ty
        lift dropScope
        return $ P.Extern ty name annotated_args ret_ty

instance Annotate1 P.Expr where
    annotate1 (P.For range init cond oper body) = do
        lift newScope
        tys <- mapM annotate1 [init, cond, oper]
        annotated_body <- annotate1 body
        let ty = P.getExprAnn annotated_body
        lift dropScope
        return $ P.For ty (tys !! 0) (tys !! 1) (tys !! 2) annotated_body
    annotate1 (P.If range cond then_body else_body) = do
        lift newScope
        annotated_cond <- annotate1 cond
        annotated_then <- annotate1 then_body
        let then_ty = P.getExprAnn annotated_then
        case else_body of
            Nothing -> do
                lift dropScope
                return $ P.If then_ty annotated_cond annotated_then Nothing
            Just block -> do
                annotated_else <- annotate1 block
                let else_ty = P.getExprAnn annotated_else
                lift dropScope
                if then_ty == else_ty
                    then return $ P.If then_ty annotated_cond annotated_then (Just annotated_else)
                    else throwE $ TypeErr $ TypeError {
                        expected = then_ty,
                        got = else_ty
                    }
    annotate1 (P.While range cond body) = do
        lift newScope
        annotated_cond <- annotate1 cond
        annotated_body <- annotate1 body
        let body_ty = P.getExprAnn annotated_body
        lift dropScope
        return $ P.While body_ty annotated_cond annotated_body
    annotate1 (P.Call range (Ann _ name) args) = do
        fun_ty <- do
            found <- lift $ gets $ \Env { fn_defs } ->
                Map.lookup name fn_defs
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        annotated_args <- mapM annotate1 args
        let args_tys = map P.getExprAnn annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ P.Call ty (Ann fun_ty name) annotated_args
            Left err -> throwE err
    annotate1 (P.Bin range (Ann _ "=") lhs rhs) = do
        annotated_rhs <- annotate1 rhs
        let ty = P.getExprAnn annotated_rhs
        case lhs of
            P.Ident range name -> do
                lift $ pushVar name ty
                return $ P.Bin ty (Ann (TFun Map.empty [ty] ty) "=") (P.Ident ty name) annotated_rhs
            _ -> throwE $ AssignErr AssignError
    annotate1 (P.Bin range (Ann _ name) lhs rhs) = do
        fun_ty <- do
            found <- lift $ gets $ \Env { bin_ops } ->
                Map.lookup name bin_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        annotated_args <- mapM annotate1 [lhs, rhs]
        let args_tys = map P.getExprAnn annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ P.Bin ty (Ann fun_ty name) (annotated_args !! 0) (annotated_args !! 1)
            Left err -> throwE err
    annotate1 (P.Un range (Ann _ name) rhs) = do
        fun_ty <- do
            found <- lift $ gets $ \Env { un_ops } ->
                Map.lookup name un_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        annotated_args <- mapM annotate1 [rhs]
        let args_tys = map P.getExprAnn annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ P.Un ty (Ann fun_ty name) (annotated_args !! 0)
            Left err -> throwE err
    annotate1 (P.Ident range ident) = do
        found <- lift $ gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (Map.lookup ident) (vars ++ [fn_defs, un_ops, bin_ops])
        maybe
            (throwE $ NotInScopeErr $ NotInScopeError { ident })
            (\ty -> return $ P.Ident ty ident)
            found
    annotate1 (P.Lit range lit@(P.IntLiteral _)) =
        return $ P.Lit T.int lit
    annotate1 (P.Lit range lit@(P.DoubleLiteral _)) =
        return $ P.Lit T.double lit
    annotate1 (P.Lit range lit@(P.BooleanLiteral _)) =
        return $ P.Lit T.bool lit
    annotate1 (P.Lit range lit@P.VoidLiteral) =
        return $ P.Lit T.void lit

implementsTraits traits ty = forM_ traits $ \trait ->
    case Map.lookup trait traitsTable of
        Nothing -> throwE $ TraitNotInScopeErr $ TraitNotInScopeError { trait }
        Just types ->
            if notElem ty types
                then throwE $ NotImplTraitErr $ NotImplTraitError { ty, trait }
                else return ()

-- TODO: Better document this function (or I won't be able to ever read it again).
type Apply = ExceptT Error (State (Map.Map TVar (Either [Trait] TCon)))
apply' :: (Type, Type) -> Apply Type
apply' (expected@(TCon _), got@(TCon _)) =
    if got == expected
        then return got
        else throwE $ TypeErr $ TypeError { expected, got }
apply' (TVar var@(TV name), got@(TCon cty)) = do
    maybe_ty <- lift $ gets $ Map.lookup var
    maybe
        (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
        ret
        maybe_ty
    where
        ret :: Either [Trait] TCon -> Apply Type
        ret (Left traits) = do
            sequence_ $ map (\trait -> maybe
                 (throwE $ TraitNotInScopeErr $ TraitNotInScopeError { trait })
                 (\types -> if notElem cty types
                    then throwE $ NotImplTraitErr $ NotImplTraitError { ty = cty, trait }
                    else return ())
                 (Map.lookup trait traitsTable)) traits
            lift $ modify $ Map.insert var $ Right cty
            return got
        ret (Right expected) =
            if TCon expected == got
                then return got
                else throwE $ TypeErr $ TypeError { expected = TCon expected, got }
apply' _ = error "Unexpected type variable, really should never happen"

-- Applies an argument list to a function, returning its result type if all types matches.
-- Supports parametric polymorphism.
-- TODO: Better document this function (or I won't be able to ever read it again).
apply :: Type -> [Type] -> Either Error Type
apply (TFun tvars t1s t2) t3s | length t1s == length t3s = do
    let zipped = zip t1s t3s
    (unified, scope) <- tvars |> Map.map Left
                              |> (zipped |> map apply' |> sequence |> runExceptT |> runState)
                              |> \(out, state) -> do { out' <- out ; return (out', state) }
    case t2 of
        TVar got@(TV name) -> maybe
            (Left $ NotInScopeErr $ NotInScopeError { ident = name })
            ret
            (Map.lookup got scope)
        TCon _ -> return t2
    where
        ret (Left traits) = error "Not completely instantied function, should never happen"
        ret (Right got) = return $ TCon got
apply (TFun _ t1s _) t3s =
    Left $ ArgCountErr $ ArgCountError { expected = length t1s, got = length t3s }

annotateAST :: P.AST a -> Either Error (P.AST Type)
annotateAST stmts =
    stmts |> mapM annotate1
          |> runExceptT
          |> flip evalState defaultEnv

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = T.builtinBinaryOps,
    un_ops = T.builtinUnaryOps,
    fn_defs = T.builtinFunctions,
    vars = [],
    tvars = Map.empty,
    count = 0
}
