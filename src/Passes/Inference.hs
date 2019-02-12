{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Passes.Inference where

import Annotation
import Errors
import Misc
import Unifier
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except

import Types as T

import qualified Data.Map as Map
import qualified Parser.Lang as P

type Scope = Map.Map Name Type
data Env = Env {
    bin_ops :: Scope,
    un_ops :: Scope,
    fn_defs :: Scope,
    vars :: [Scope]
} deriving (Show, Eq)
type Inferred = ExceptT Error (State Env)

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

class Infer a where
    infer :: a -> Inferred Type

instance Unify Type where
    unify t1@(TCon _) t2@(TCon _) | t1 == t2 = Just t1
    unify _ _ = Nothing

instance Infer P.Type where
    infer P.IntType = return T.int
    infer P.FloatType = return T.float
    infer P.VoidType = return T.void

instance Infer P.Literal where
    infer (P.IntLiteral _) = return T.int
    infer (P.FloatLiteral _) = return T.float
    infer P.VoidLiteral = return T.void

instance Infer (P.Stmt ()) where
    infer (P.DefnStmt (Ann _ defn)) = infer defn
    infer (P.ExprStmt (Ann _ expr)) = infer expr

instance Infer (P.Defn ()) where
    infer (P.Op (Ann _ defn)) = infer defn
    infer (P.Fn (Ann _ expr)) = infer expr

-- TODO: Check if op already exists (in any scope ?).
instance Infer (P.OpDefn ()) where
    infer P.OpDefn {
        P.opdefn_op = op,
        P.opdefn_arity = arity,
        P.opdefn_args = args,
        P.opdefn_ret_ty = ret_ty,
        P.opdefn_body = Ann _ body
    } = do
        lift newScope
        tys <- args |> map (\(Ann _ a) -> infer a) |> sequence
        expected <- infer ret_ty
        let ty = TFun Map.empty tys expected
        case arity of
            P.Binary -> do
                maybe_op <- lift $ getBinOp op
                case maybe_op of
                    Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                        name = op,
                        definitions = [ty2, ty]
                    }
                    Nothing -> lift $ pushBinOp op ty
            P.Unary -> do
                maybe_op <- lift $ getUnOp op
                case maybe_op of
                    Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                        name = op,
                        definitions = [ty2, ty]
                    }
                    Nothing -> lift $ pushUnOp op ty
        inferred <- infer body
        lift dropScope
        if inferred == expected
            then return ty
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Infer P.Arg where
    infer P.Arg { P.arg_name = name, P.arg_type = arg_ty } = do
        ty <- infer arg_ty
        lift $ pushVar name ty
        return ty

-- TODO: Check if fn already exists (in any scope ?).
instance Infer (P.FnDefn ()) where
    infer P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = Ann _ body
    } = do
        lift newScope
        tys <- args |> map (\(Ann _ a) -> infer a) |> sequence
        expected <- infer ret_ty
        let ty = TFun Map.empty tys expected
        maybe_fn <- lift $ getFnDef name
        case maybe_fn of
            Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                name, definitions = [ty2, ty]
            }
            Nothing -> lift $ pushFnDef name ty
        inferred <- infer body
        lift dropScope
        if inferred == expected
            then return ty
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Infer (P.ForExpr ()) where
    infer P.ForExpr {
        P.for_init = Ann _ init,
        P.for_cond = Ann _ cond,
        P.for_oper = Ann _ oper,
        P.for_body = Ann _ body
    } = do
        lift newScope
        tys <- [init, cond, oper] |> map infer |> sequence
        ty <- infer body
        lift dropScope
        return ty

instance Infer (P.IfExpr ()) where
    infer P.IfExpr {
        P.if_cond = Ann _ cond,
        P.if_then = Ann _ body,
        P.if_else = else_block
    } = do
        lift newScope
        cond_ty <- infer cond
        then_ty <- infer body
        maybe (do { lift dropScope ; return then_ty })
            (\(Ann _ block) -> do
                else_ty <- infer block
                lift dropScope
                if then_ty == else_ty
                    then return then_ty
                    else throwE $ TypeErr $ TypeError {
                        expected = then_ty,
                        got = else_ty
                    })
            else_block

instance Infer (P.WhileExpr ()) where
    infer P.WhileExpr {
        P.while_cond = (Ann _ cond),
        P.while_body = (Ann _ body)
    } = do
        lift newScope
        cond_ty <- infer cond
        body_ty <- infer body
        lift dropScope
        return body_ty

instance Infer (P.CallExpr ()) where
    infer P.CallExpr {
        P.call_ident = name,
        P.call_args = args
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { fn_defs } ->
                Map.lookup name fn_defs
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- args |> map (\(Ann _ a) -> infer a) |> sequence
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.BinExpr ()) where
    -- TODO: Check if ${lhs} already exists (in current scope only).
    infer P.BinExpr {
        P.bin_op = "=",
        P.bin_lhs = Ann _ lhs,
        P.bin_rhs = Ann _ rhs
    } = do
        ty <- infer rhs
        case lhs of
            P.Ident (Ann _ name) -> do
                lift $ pushVar name ty
                return ty
            _ -> throwE $ AssignErr AssignError
    infer P.BinExpr {
        P.bin_op = name,
        P.bin_lhs = Ann _ lhs,
        P.bin_rhs = Ann _ rhs
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { bin_ops } ->
                Map.lookup name bin_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- [lhs, rhs] |> map infer |> sequence
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.UnExpr ()) where
    infer P.UnExpr {
        P.un_op = name,
        P.un_rhs = Ann _ arg
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { un_ops } ->
                Map.lookup name un_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- [arg] |> map infer |> sequence
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.Expr ()) where
    infer (P.For (Ann _ forExpr)) = infer forExpr
    infer (P.If (Ann _ ifExpr)) = infer ifExpr
    infer (P.While (Ann _ whileExpr)) = infer whileExpr
    infer (P.Ident (Ann _ ident)) = do
        found <- lift $ gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (Map.lookup ident) (vars ++ [fn_defs, un_ops, bin_ops])
        maybe
            (throwE $ NotInScopeErr $ NotInScopeError { ident })
            return
            found
    infer (P.Lit (Ann _ literal)) = infer literal
    infer (P.Call (Ann _ callExpr)) = infer callExpr
    infer (P.Un (Ann _ unExpr)) = infer unExpr
    infer (P.Bin (Ann _ binExpr)) = infer binExpr

-- TODO: Better document this function (or I won't be able to ever read it again).
type Apply = ExceptT Error (State (Map.Map TVar (Either [Trait] TCon)))
apply' :: (Type, Type) -> Apply Type
apply' (expected@(TCon _), got@(TCon _)) =
    maybe
        (throwE $ TypeErr $ TypeError { expected, got })
        return
        (got <-> expected)
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
        ret (Right expected) = maybe
            (throwE $ TypeErr $ TypeError { expected = TCon expected, got })
            return
            (TCon expected <-> got)
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

inferAST :: P.AST () -> Either Error [Type]
inferAST stmts =
    stmts |> map (\(Ann _ a) -> infer a)
          |> sequence
          |> runExceptT
          |> flip evalState defaultEnv

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = T.builtinBinaryOps,
    un_ops = T.builtinUnaryOps,
    fn_defs = Map.empty,
    vars = []
}
