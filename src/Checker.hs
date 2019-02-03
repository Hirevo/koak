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
data Type =
    TInt
    | TFloat
    | TVoid
    | TFun [Type] Type
    | TUni [Type]
    deriving (Eq)
instance Show Type where
    show TInt = "int"
    show TFloat = "double"
    show TVoid = "void"
    show (TFun args ret) = "(" ++ concat (intersperse ", " $ map show args) ++ ") -> " ++ show ret
    show (TUni tys) =
        concat (intersperse " | "
            $ map (\x -> if isFun x then "(" ++ show x ++ ")" else show x) tys)

isFun :: Type -> Bool
isFun (TFun _ _) = True
isFun _ = False

data Typed a =
    Typed Type a
    deriving (Show, Eq, Functor)

type Scope = [(String, Type)]
data Env = Env {
    bin_ops :: Scope,
    un_ops :: Scope,
    fn_defs :: Scope,
    vars :: [Scope]
} deriving (Show, Eq)
type Inferred = ExceptT Error (State Env)

class Infer a where
    infer :: a -> Inferred Type

instance Unify Type where
    unify ty (TUni tys) | elem ty tys = Just ty
    unify t1 t2 | t1 == t2 = Just t1
    unify _ _ = Nothing

instance Infer P.Type where
    infer P.IntType = return TInt
    infer P.FloatType = return TFloat
    infer P.VoidType = return TVoid

instance Infer P.Literal where
    infer (P.IntLiteral _) = return TInt
    infer (P.FloatLiteral _) = return TFloat
    infer P.VoidLiteral = return TVoid

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
        P.opdefn_body = (P.Untyped body)
    } = do
        lift $ modify $ \env -> env { vars = [] : vars env }
        tys <- sequence $ map (\(P.Untyped a) -> infer a) args
        expected <- infer ret_ty
        inferred <- infer body
        if inferred == expected
            then do
                let ty = TFun tys expected
                case arity of
                    P.Binary -> lift $ modify $ \env -> env {
                        bin_ops = (op, ty) : bin_ops env
                    }
                    P.Unary -> lift $ modify $ \env -> env {
                        un_ops = (op, ty) : un_ops env
                    }
                return ty
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Infer P.Arg where
    infer P.Arg { P.arg_name = name, P.arg_type = arg_ty } = do
        ty <- infer arg_ty
        lift $ modify $ \env -> env {
            vars = ((name, ty) : head (vars env)) : tail (vars env)
        }
        return ty

-- TODO: Check if fn already exists (in any scope ?).
instance Infer (P.FnDefn P.Untyped) where
    infer P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = (P.Untyped body)
    } = do
        tys <- sequence $ map (\(P.Untyped a) -> infer a) args
        expected <- infer ret_ty
        let ty = TFun tys expected
        lift $ modify $ \env -> env {
            fn_defs = (name, ty) : fn_defs env,
            vars = [] : vars env
        }
        inferred <- infer body
        lift $ modify $ \env -> env { vars = tail $ vars env }
        if inferred == expected
            then do
                lift $ modify $ \env -> env {
                    vars = ((name, ty) : head (vars env)) : tail (vars env)
                }
                return ty
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Infer (P.ForExpr P.Untyped) where
    infer P.ForExpr {
        P.for_init = (P.Untyped init),
        P.for_cond = (P.Untyped cond),
        P.for_oper = (P.Untyped oper),
        P.for_body = (P.Untyped body)
    } = do
        lift $ modify $ \env -> env { vars = [] : vars env }
        tys <- sequence $ map infer [init, cond, oper]
        ty <- infer body
        lift $ modify $ \env -> env { vars = tail $ vars env }
        return ty

instance Infer (P.IfExpr P.Untyped) where
    infer P.IfExpr {
        P.if_cond = (P.Untyped cond),
        P.if_then = (P.Untyped body),
        P.if_else = else_block
    } = do
        lift $ modify $ \env -> env { vars = [] : vars env }
        cond_ty <- infer cond
        then_ty <- infer body
        maybe (do { lift $ modify $ \env -> env { vars = tail $ vars env } ; return then_ty })
            (\(P.Untyped block) -> do
                else_ty <- infer block
                lift $ modify $ \env -> env { vars = tail $ vars env }
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
        lift $ modify $ \env -> env { vars = [] : vars env }
        cond_ty <- infer cond
        body_ty <- infer body
        lift $ modify $ \env -> env { vars = tail $ vars env }
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
                lift $ modify $ \env -> env { vars = ((name, ty) : head (vars env)) : tail (vars env) }
                return ty
            _ -> throwE $ AssignErr AssignError
    infer P.BinExpr {
        P.bin_op = name,
        P.bin_lhs = lhs,
        P.bin_rhs = rhs
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { bin_ops } ->
                lookup name bin_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map (\(P.Untyped a) -> infer a) [lhs, rhs]
        case apply fun_ty args_tys of
            Right ty -> return ty
            Left err -> throwE err

instance Infer (P.UnExpr P.Untyped) where
    infer P.UnExpr {
        P.un_op = name,
        P.un_arg = arg
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { un_ops } ->
                lookup name un_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map (\(P.Untyped a) -> infer a) [arg]
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

-- Applies an argument list to a function, returning its result type if all types matches.
-- Supports union types.
apply :: Type -> [Type] -> Either Error Type
apply (TFun t1s t2) t3s | length t1s == length t3s = do
    let zipped = zip t1s t3s
    let unified = map (uncurry $ flip (<->)) zipped
    if all isJust unified
        then Right t2
        else let idx = fromJust $ findIndex isNothing unified
                 (expected, got) = zipped !! idx
        in Left $ TypeErr $ TypeError { expected, got }
apply (TFun t1s t2) t3s = Left $ ArgCountErr $ ArgCountError { expected = length t1s, got = length t3s }
apply (TUni ts) t3s | all isFun ts =
    let ret = map (`apply` t3s) ts
    in fromMaybe (fromJust $ find isLeft ret) (find isRight ret)

inferAST :: P.AST P.Untyped -> Either Error [Type]
inferAST ast = evalState (runExceptT $ sequence $ map (\(P.Untyped a) -> infer a) ast) defaultEnv

defaultEnv :: Env
defaultEnv = Env {
    bin_ops = [
        ( "+", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat]),
        ( "-", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat]),
        ( "*", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat]),
        ( "/", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat]),
        ( "<", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat]   TInt]),
        ( ">", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat]   TInt]),
        ("==", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat]   TInt]),
        ("!=", TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat]   TInt]),
        ( ":", TUni [TFun [TUni [TInt, TFloat], TInt] TInt, TFun [TUni [TInt, TFloat], TFloat] TFloat])
    ],
    un_ops = [
        ("!", TUni [TFun [TInt] TInt, TFun [TFloat] TInt]),
        ("-", TUni [TFun [TInt] TInt, TFun [TFloat] TFloat])
    ],
    fn_defs = [],
    vars = []
}
