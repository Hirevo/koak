{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Checker where

import Unifier
import qualified Parser.Lang as P
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative
import Data.List (find, findIndex, intersperse)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Either (isRight, isLeft)

data Error =
    TypeErr TypeError
    | ArgCountErr ArgCountError
    | NotInScopeErr NotInScopeError
    | AssignErr AssignError
    deriving (Eq)
instance Show Error where
    show (TypeErr err) = "error: " ++ show err
    show (ArgCountErr err) = "error: " ++ show err
    show (NotInScopeErr err) = "error: " ++ show err
    show (AssignErr err) = "error: " ++ show err
data TypeError = TypeError {
    expected :: Type,
    got :: Type
} deriving (Eq)
instance Show TypeError where
    show TypeError { expected, got } =
        "TypeError (expected: " ++ show expected
            ++ ", got: " ++ show got ++ ")"
data ArgCountError = ArgCountError {
    expected :: Int,
    got :: Int
} deriving (Eq)
instance Show ArgCountError where
    show ArgCountError { expected, got } =
        "ArgCountError (expected: " ++ show expected
            ++ ", got: " ++ show got ++ ")"
newtype NotInScopeError = NotInScopeError {
    ident :: Name
} deriving (Eq)
instance Show NotInScopeError where
    show NotInScopeError { ident } = "NotInScopeError: '" ++ ident ++ "'"
data AssignError = AssignError deriving (Eq)
instance Show AssignError where
    show AssignError =
        "AssignError (expected identifier on the left-hand side of an assignment)"

type Name = String
data ConcreteType =
    TInt
    | TFloat
    | TVoid
    | TFun [Name] [Type] Type
    deriving (Eq)
instance Show ConcreteType where
    show TInt = "int"
    show TFloat = "double"
    show TVoid = "void"
    show (TFun tvars args ret) =
        "<" ++ concat (intersperse ", " $ map ('\'' :) tvars) ++ ">"
        ++ "(" ++ concat (intersperse ", " $ map show args) ++ ") -> "
        ++ show ret
data Type =
    TCon ConcreteType
    | TVar Name
    deriving (Eq)
instance Show Type where
    show (TCon ty) = show ty
    show (TVar name) = '\'' : name

isFun :: Type -> Bool
isFun (TCon TFun{}) = True
isFun _ = False

data Typed a =
    Typed Type a
    deriving (Show, Eq, Functor)

type Scope = [(Name, Type)]
data Env = Env {
    bin_ops :: Scope,
    un_ops :: Scope,
    fn_defs :: Scope,
    vars :: [Scope]
} deriving (Show, Eq)
type Inferred = ExceptT Error (State Env)

pushVar :: (Name, Type) -> State Env ()
pushVar def = modify $ \env ->
    if null $ vars env
        then env { vars = [[def]] }
        else env { vars = (def : head (vars env)) : tail (vars env) }

newScope :: State Env ()
newScope = modify $ \env -> env { vars = [] : vars env }

dropScope :: State Env ()
dropScope = modify $ \env -> env { vars = tail $ vars env }

pushFnDef :: (Name, Type) -> State Env ()
pushFnDef def = modify $ \env -> env { fn_defs = def : fn_defs env }

pushBinOp :: (Name, Type) -> State Env ()
pushBinOp def = modify $ \env -> env { bin_ops = def : bin_ops env }

pushUnOp :: (Name, Type) -> State Env ()
pushUnOp def = modify $ \env -> env { un_ops = def : un_ops env }

class Infer a where
    infer :: a -> Inferred Type

instance Unify Type where
    unify t1@(TCon _) t2@(TCon _) | t1 == t2 = Just t1
    unify _ _ = Nothing

instance Infer P.Type where
    infer P.IntType = return $ TCon TInt
    infer P.FloatType = return $ TCon TFloat
    infer P.VoidType = return $ TCon TVoid

instance Infer P.Literal where
    infer (P.IntLiteral _) = return $ TCon TInt
    infer (P.FloatLiteral _) = return $ TCon TFloat
    infer P.VoidLiteral = return $ TCon TVoid

instance Infer (P.Stmt P.Untyped) where
    infer (P.DefnStmt (P.Untyped defn)) = infer defn
    infer (P.ExprStmt (P.Untyped expr)) = infer expr

instance Infer (P.Defn P.Untyped) where
    infer (P.Op (P.Untyped defn)) = infer defn
    infer (P.Fn (P.Untyped expr)) = infer expr

-- TODO: Check if op already exists (in any scope ?).
instance Infer (P.OpDefn P.Untyped) where
    infer P.OpDefn {
        P.opdefn_op = op,
        P.opdefn_arity = arity,
        P.opdefn_args = args,
        P.opdefn_ret_ty = ret_ty,
        P.opdefn_body = P.Untyped body
    } = do
        lift newScope
        tys <- sequence $ map (\(P.Untyped a) -> infer a) args
        expected <- infer ret_ty
        let ty = TCon $ TFun [] tys expected
        case arity of
            P.Binary -> lift $ pushBinOp (op, ty)
            P.Unary -> lift $ pushUnOp (op, ty)
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
        lift $ pushVar (name, ty)
        return ty

-- TODO: Check if fn already exists (in any scope ?).
instance Infer (P.FnDefn P.Untyped) where
    infer P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = P.Untyped body
    } = do
        lift newScope
        tys <- sequence $ map (\(P.Untyped a) -> infer a) args
        expected <- infer ret_ty
        let ty = TCon $ TFun [] tys expected
        lift $ pushFnDef (name, ty)
        inferred <- infer body
        lift dropScope
        if inferred == expected
            then return ty
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Infer (P.ForExpr P.Untyped) where
    infer P.ForExpr {
        P.for_init = P.Untyped init,
        P.for_cond = P.Untyped cond,
        P.for_oper = P.Untyped oper,
        P.for_body = P.Untyped body
    } = do
        lift newScope
        tys <- sequence $ map infer [init, cond, oper]
        ty <- infer body
        lift dropScope
        return ty

instance Infer (P.IfExpr P.Untyped) where
    infer P.IfExpr {
        P.if_cond = P.Untyped cond,
        P.if_then = P.Untyped body,
        P.if_else = else_block
    } = do
        lift newScope
        cond_ty <- infer cond
        then_ty <- infer body
        maybe (do { lift dropScope ; return then_ty })
            (\(P.Untyped block) -> do
                else_ty <- infer block
                lift dropScope
                if then_ty == else_ty
                    then return then_ty
                    else throwE $ TypeErr $ TypeError {
                        expected = then_ty,
                        got = else_ty
                    })
            else_block

instance Infer (P.WhileExpr P.Untyped) where
    infer P.WhileExpr {
        P.while_cond = (P.Untyped cond),
        P.while_body = (P.Untyped body)
    } = do
        lift newScope
        cond_ty <- infer cond
        body_ty <- infer body
        lift dropScope
        return body_ty

instance Infer (P.CallExpr P.Untyped) where
    infer P.CallExpr {
        P.call_ident = name,
        P.call_args = args
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { fn_defs } ->
                lookup name fn_defs
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map (\(P.Untyped a) -> infer a) args
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.BinExpr P.Untyped) where
    -- TODO: Check if ${lhs} already exists (in current scope only).
    infer P.BinExpr {
        P.bin_op = "=",
        P.bin_lhs = P.Untyped lhs,
        P.bin_rhs = P.Untyped rhs
    } = do
        ty <- infer rhs
        case lhs of
            P.Ident (P.Untyped name) -> do
                lift $ pushVar (name, ty)
                return ty
            _ -> throwE $ AssignErr AssignError
    infer P.BinExpr {
        P.bin_op = name,
        P.bin_lhs = P.Untyped lhs,
        P.bin_rhs = P.Untyped rhs
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { bin_ops } ->
                lookup name bin_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map infer [lhs, rhs]
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.UnExpr P.Untyped) where
    infer P.UnExpr {
        P.un_op = name,
        P.un_arg = P.Untyped arg
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { un_ops } ->
                lookup name un_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map infer [arg]
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.Expr P.Untyped) where
    infer (P.For (P.Untyped forExpr)) = infer forExpr
    infer (P.If (P.Untyped ifExpr)) = infer ifExpr
    infer (P.While (P.Untyped whileExpr)) = infer whileExpr
    infer (P.Ident (P.Untyped ident)) = do
        found <- lift $ gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (lookup ident) (vars ++ [fn_defs, un_ops, bin_ops])
        maybe
            (throwE $ NotInScopeErr $ NotInScopeError { ident })
            return
            found
    infer (P.Lit (P.Untyped literal)) = infer literal
    infer (P.Call (P.Untyped callExpr)) = infer callExpr
    infer (P.Un (P.Untyped unExpr)) = infer unExpr
    infer (P.Bin (P.Untyped binExpr)) = infer binExpr

apply' :: (Type, Type) -> ExceptT Error (State Scope) Type
apply' (TVar n, expected@(TCon _)) = do
    maybe_ty <- lift $ gets $ lookup n
    maybe
        (do { modify $ \scope -> (n, expected) : scope ; return expected })
        (\got -> maybe
            (throwE $ TypeErr $ TypeError { expected, got })
            return
            (got <-> expected))
        maybe_ty
apply' (got@(TCon _), expected@(TCon _)) =
    maybe
        (throwE $ TypeErr $ TypeError { expected, got })
        return
        (got <-> expected)

-- Applies an argument list to a function, returning its result type if all types matches.
-- Supports parametric polymorphism.
apply :: Type -> [Type] -> Either Error Type
apply (TCon (TFun tvars t1s t2)) t3s | length t1s == length t3s = do
    let zipped = zip t1s t3s
    (unified, scope) <- (\(out, state) -> do { out' <- out ; return (out', state) })
        $ runState (runExceptT $ sequence $ map apply' zipped) []
    case t2 of
        TVar n -> maybe
            (Left $ NotInScopeErr $ NotInScopeError { ident = n })
            return
            (lookup n scope)
        TCon _ -> return t2
apply (TCon (TFun _ t1s _)) t3s =
    Left $ ArgCountErr $ ArgCountError { expected = length t1s, got = length t3s }

inferAST :: P.AST P.Untyped -> Either Error [Type]
inferAST ast =
    let inferred = sequence $ map (\(P.Untyped a) -> infer a) ast
    in evalState (runExceptT inferred) defaultEnv

-- TODO: AST annotation
-- annotateAST :: P.AST P.Untyped -> Either Error (P.AST Typed)
-- annotateAST elem =
--     let types = sequence $ map (\(P.Untyped a) -> infer a) ast -- ExceptT (State Env) [Type]

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = [
        ( "+", TCon (TFun ["a"] [TVar "a", TVar "a"] (TVar "a"))),
        ( "-", TCon (TFun ["a"] [TVar "a", TVar "a"] (TVar "a"))),
        ( "*", TCon (TFun ["a"] [TVar "a", TVar "a"] (TVar "a"))),
        ( "/", TCon (TFun ["a"] [TVar "a", TVar "a"] (TVar "a"))),
        ( "<", TCon (TFun ["a"] [TVar "a", TVar "a"] (TCon TInt))),
        ( ">", TCon (TFun ["a"] [TVar "a", TVar "a"] (TCon TInt))),
        ("==", TCon (TFun ["a"] [TVar "a", TVar "a"] (TCon TInt))),
        ("!=", TCon (TFun ["a"] [TVar "a", TVar "a"] (TCon TInt))),
        ( ":", TCon (TFun ["a", "b"] [TVar "a", TVar "b"] (TVar "b")))
    ],
    un_ops = [
        ("!", TCon (TFun ["a"] [TVar "a"] (TCon TInt))),
        ("-", TCon (TFun ["a"] [TVar "a"] (TVar "a")))
    ],
    fn_defs = [],
    vars = []
}
