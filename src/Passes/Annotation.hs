{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Passes.Annotation where

import Annotation
import Errors
import Misc
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State.Lazy

import Types as T

import qualified Data.Map as Map
import qualified Parser.Lib as L
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
newtype Annotate a = Annotate {
    unAnnotate :: ExceptT Error (State Env) a
} deriving (Functor, Applicative, Monad, MonadFix, MonadState Env, MonadError Error)

newScope, dropScope :: MonadState Env m => m ()
newScope = modify $ \env -> env { vars = Map.empty : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }
withScope :: MonadState Env m => m a -> m a
withScope action = do
    newScope
    ret <- action
    dropScope
    return ret

pushFnDef, pushBinOp, pushUnOp, pushVar :: MonadState Env m => Name -> Type -> m ()
pushFnDef name ty = modify $ \env -> env { fn_defs = Map.insert name ty $ fn_defs env }
pushBinOp name ty = modify $ \env -> env { bin_ops = Map.insert name ty $ bin_ops env }
pushUnOp name ty = modify $ \env -> env { un_ops = Map.insert name ty $ un_ops env }
pushVar name ty = modify $ \env ->
    if null $ vars env
        then env { vars = [Map.singleton name ty] }
        else env { vars = Map.insert name ty (head $ vars env) : tail (vars env) }

getFnDef, getBinOp, getUnOp, getVar :: MonadState Env m => Name -> m (Maybe Type)
getFnDef name = gets $ \env -> env |> fn_defs |> Map.lookup name
getBinOp name = gets $ \env -> env |> bin_ops |> Map.lookup name
getUnOp name = gets $ \env -> env |> un_ops |> Map.lookup name
getVar name = gets $ \env -> env |> vars |> map (Map.lookup name) |> foldl1 (<|>)

annotateArg :: Ann L.Range P.Arg -> Annotate (Ann (L.Range, Type) P.Arg)
annotateArg (Ann range (P.Arg name ty)) = do
    pushVar name ty
    return $ Ann (range, ty) $ P.Arg name ty

annotateStmt :: Ann L.Range (P.Stmt L.Range) -> Annotate (Ann (L.Range, Type) (P.Stmt (L.Range, Type)))
annotateStmt = \case
    Ann range (P.Defn defn_ty name args ret_ty body) ->
        withScope $ do
            annotated_args <- mapM annotateArg args
            let tys = map (snd . annotation) annotated_args
            let ty = TFun Map.empty tys ret_ty
            let (getf, pushf) = case defn_ty of
                 P.Function -> (getFnDef, pushFnDef)
                 P.Unary _ -> (getUnOp, pushUnOp)
                 P.Binary _ -> (getBinOp, pushBinOp)
            maybe_fn <- getf name
            case maybe_fn of
                Just ty2 -> throwError $ MultipleDefnError name [ty2, ty]
                Nothing -> pushf name ty
            annotated_body <- annotateExpr body
            let inferred = snd (annotation annotated_body)
            if inferred == ret_ty
                then return $ Ann (range, ty) $ P.Defn defn_ty name annotated_args ret_ty annotated_body
                else throwError $ TypeError ret_ty inferred
    Ann range (P.Expr expr) -> do
        annotated_expr <- annotateExpr expr
        return $ Ann (range, snd (annotation annotated_expr)) $ P.Expr annotated_expr
    Ann range (P.Extern name args ret_ty) ->
        withScope $ do
            annotated_args <- mapM annotateArg args
            let tys = map (snd . annotation) annotated_args
            let ty = TFun Map.empty tys ret_ty
            maybe_fn <- getFnDef name
            case maybe_fn of
                Just ty2 -> throwError $ MultipleDefnError name [ty2, ty]
                Nothing -> pushFnDef name ty
            return $ Ann (range, ty) $ P.Extern name annotated_args ret_ty

annotateExpr :: Ann L.Range (P.Expr L.Range) -> Annotate (Ann (L.Range, Type) (P.Expr (L.Range, Type)))
annotateExpr = \case
    Ann range (P.For init cond oper body) ->
        withScope $ do
            annotated_init <- annotateExpr init
            annotated_cond <- annotateExpr cond
            annotated_oper <- annotateExpr oper
            annotated_body <- annotateExpr body
            return $ Ann (range, snd (annotation annotated_body))
                   $ P.For annotated_init annotated_cond annotated_oper annotated_body
    Ann range (P.If cond then_body else_body) ->
        withScope $ do
            annotated_cond <- annotateExpr cond
            annotated_then <- annotateExpr then_body
            let then_ty = snd (annotation annotated_then)
            case else_body of
                Nothing -> return $ Ann (range, then_ty)
                                  $ P.If annotated_cond annotated_then Nothing
                Just block -> do
                    annotated_else <- annotateExpr block
                    let else_ty = snd (annotation annotated_else)
                    if then_ty == else_ty
                        then return $ Ann (range, then_ty)
                                    $ P.If annotated_cond annotated_then (Just annotated_else)
                        else throwError $ TypeError then_ty else_ty
    Ann range (P.While cond body) ->
        withScope $ do
            annotated_cond <- annotateExpr cond
            annotated_body <- annotateExpr body
            return $ Ann (range, snd (annotation annotated_body))
                   $ P.While annotated_cond annotated_body
    Ann range (P.Call (Ann range2 name) args) -> do
        fun_ty <- do
            found <- gets $ \Env { fn_defs } ->
                Map.lookup name fn_defs
            case found of
                Nothing -> throwError $ NotInScopeError name
                Just elem -> return elem
        annotated_args <- mapM annotateExpr args
        let args_tys = map (snd . annotation) annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ Ann (range, ty)
                               $ P.Call (Ann (range2, fun_ty) name) annotated_args
            Left err -> throwError err
    Ann range (P.Bin (Ann range2 "=") lhs rhs) -> do
        annotated_rhs <- annotateExpr rhs
        let ty = snd (annotation annotated_rhs)
        case lhs of
            Ann range3 (P.Ident name) -> do
                pushVar name ty
                return $ Ann (range, ty)
                       $ P.Bin (Ann (range2, TFun Map.empty [ty] ty) "=") (Ann (range3, ty) (P.Ident name)) annotated_rhs
            _ -> throwError AssignError
    Ann range (P.Bin (Ann range2 name) lhs rhs) -> do
        fun_ty <- do
            found <- gets $ \Env { bin_ops } ->
                Map.lookup name bin_ops
            case found of
                Nothing -> throwError $ NotInScopeError name
                Just elem -> return elem
        annotated_lhs <- annotateExpr lhs
        annotated_rhs <- annotateExpr rhs
        case apply fun_ty [snd (annotation annotated_lhs), snd (annotation annotated_rhs)] of
            Right ty -> return $ Ann (range, ty)
                               $ P.Bin (Ann (range2, fun_ty) name) annotated_lhs annotated_rhs
            Left err -> throwError err
    Ann range (P.Un (Ann range2 name) rhs) -> do
        fun_ty <- do
            found <- gets $ \Env { un_ops } ->
                Map.lookup name un_ops
            case found of
                Nothing -> throwError $ NotInScopeError name
                Just elem -> return elem
        annotated_rhs <- annotateExpr rhs
        case apply fun_ty [snd (annotation annotated_rhs)] of
            Right ty -> return $ Ann (range, ty)
                               $ P.Un (Ann (range2, fun_ty) name) annotated_rhs
            Left err -> throwError err
    Ann range (P.Ident ident) -> do
        found <- gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (Map.lookup ident) (vars <> [fn_defs, un_ops, bin_ops])
        case found of
            Nothing -> throwError $ NotInScopeError ident
            Just ty -> return $ Ann (range, ty)
                              $ P.Ident ident
    Ann range (P.Lit lit@(P.IntLiteral _)) -> return $ Ann (range, T.int) $ P.Lit lit
    Ann range (P.Lit lit@(P.DoubleLiteral _)) -> return $ Ann (range, T.double) $ P.Lit lit
    Ann range (P.Lit lit@(P.BooleanLiteral _)) -> return $ Ann (range, T.bool) $ P.Lit lit
    Ann range (P.Lit lit@P.VoidLiteral) -> return $ Ann (range, T.void) $ P.Lit lit

implementsTraits traits ty = forM_ traits $ \trait ->
    case Map.lookup trait traitsTable of
        Nothing -> throwError $ TraitNotInScopeError trait
        Just types ->
            if notElem ty types
                then throwError $ NotImplTraitError (TCon ty) trait
                else return ()

-- TODO: Better document this function (or I won't be able to ever read it again).
type Apply = ExceptT Error (State (Map.Map TVar (Either [Trait] TCon)))
apply' :: (Type, Type) -> Apply Type
apply' (expected@(TCon _), got@(TCon _)) =
    if got == expected
        then return got
        else throwError $ TypeError expected got
apply' (TVar var@(TV name), got@(TCon cty)) = do
    maybe_ty <- gets $ Map.lookup var
    case maybe_ty of
        Nothing -> throwError $ NotInScopeError name
        Just ty -> ret ty
    where
        ret :: Either [Trait] TCon -> Apply Type
        ret = \case
            Left traits -> do
                forM_ traits $ \trait ->
                    case Map.lookup trait traitsTable of
                        Nothing -> throwError (TraitNotInScopeError trait)
                        Just types ->
                            if notElem cty types
                                then throwError $ NotImplTraitError (TCon cty) trait
                                else return ()
                modify $ Map.insert var $ Right cty
                return got
            Right expected ->
                if TCon expected == got
                    then return got
                    else throwError $ TypeError (TCon expected) got
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
        TVar got@(TV name) ->
            case Map.lookup got scope of
                Nothing -> Left (NotInScopeError name)
                Just ty -> ret ty
        TCon _ -> return t2
    where
        ret (Left traits) = error "Not completely instantied function, should never happen"
        ret (Right got) = return $ TCon got
apply (TFun _ t1s _) t3s =
    Left $ ArgCountError (length t1s) (length t3s)

annotateAST :: P.AST L.Range -> Either Error (P.AST (L.Range, Type))
annotateAST stmts =
    stmts |> mapM annotateStmt
          |> unAnnotate
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
