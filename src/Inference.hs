{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Inference where

import Annotation
import Misc
import Unifier
import Misc
import qualified Parser.Lang as P
import Types as T
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Applicative
import Data.List (find, findIndex, intersperse)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Either (isRight, isLeft)
import qualified Data.Map as Map

data Error =
    TypeErr TypeError
    | ArgCountErr ArgCountError
    | NotInScopeErr NotInScopeError
    | AssignErr AssignError
    | NotImplTraitErr NotImplTraitError
    | TraitNotInScopeErr TraitNotInScopeError
    deriving (Eq)
instance Show Error where
    show (TypeErr err) = show err
    show (ArgCountErr err) = show err
    show (NotInScopeErr err) = show err
    show (AssignErr err) = show err
    show (NotImplTraitErr err) = show err
    show (TraitNotInScopeErr err) = show err
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
data NotImplTraitError = NotImplTraitError {
    ty :: TCon,
    trait :: Trait
} deriving (Eq)
instance Show NotImplTraitError where
    show NotImplTraitError { ty, trait } =
        "NotImplTraitError: The type '" ++ show ty ++ "' does not implement the '"
            ++ show trait ++ "' trait."
newtype TraitNotInScopeError = TraitNotInScopeError {
    trait :: Trait
} deriving (Eq)
instance Show TraitNotInScopeError where
    show TraitNotInScopeError { trait } =
        "TraitNotInScopeError: The trait '" ++ show trait ++ "' is not defined."

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

newScope, dropScope :: State Env ()
newScope = modify $ \env -> env { vars = [] : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }

pushFnDef, pushBinOp, pushUnOp :: (Name, Type) -> State Env ()
pushFnDef def = modify $ \env -> env { fn_defs = def : fn_defs env }
pushBinOp def = modify $ \env -> env { bin_ops = def : bin_ops env }
pushUnOp def = modify $ \env -> env { un_ops = def : un_ops env }

traitsTable :: Map.Map Trait [TCon]
traitsTable = Map.fromList [ (Trait "Num", [TC "int", TC "double"])
                           , (Trait "Eq",  [TC "int", TC "double", TC "void"])
                           , (Trait "Ord", [TC "int", TC "double"]) ]

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
        tys <- sequence $ map (\(Ann _ a) -> infer a) args
        expected <- infer ret_ty
        let ty = TFun Map.empty tys expected
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
instance Infer (P.FnDefn ()) where
    infer P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = Ann _ body
    } = do
        lift newScope
        tys <- sequence $ map (\(Ann _ a) -> infer a) args
        expected <- infer ret_ty
        let ty = TFun Map.empty tys expected
        lift $ pushFnDef (name, ty)
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
        tys <- sequence $ map infer [init, cond, oper]
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
                lookup name fn_defs
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        args_tys <- sequence $ map (\(Ann _ a) -> infer a) args
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
                lift $ pushVar (name, ty)
                return ty
            _ -> throwE $ AssignErr AssignError
    infer P.BinExpr {
        P.bin_op = name,
        P.bin_lhs = Ann _ lhs,
        P.bin_rhs = Ann _ rhs
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

instance Infer (P.UnExpr ()) where
    infer P.UnExpr {
        P.un_op = name,
        P.un_rhs = Ann _ arg
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

instance Infer (P.Expr ()) where
    infer (P.For (Ann _ forExpr)) = infer forExpr
    infer (P.If (Ann _ ifExpr)) = infer ifExpr
    infer (P.While (Ann _ whileExpr)) = infer whileExpr
    infer (P.Ident (Ann _ ident)) = do
        found <- lift $ gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (lookup ident) (vars ++ [fn_defs, un_ops, bin_ops])
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
    (unified, scope) <- tvars
        |> Map.map Left
        |> runState (runExceptT $ sequence $ map apply' zipped)
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
    bin_ops = [
        ( "+", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "-", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "*", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "/", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "<", TFun (Map.fromList [(TV "T", [Trait "Ord"])]) [TVar $ TV "T", TVar $ TV "T"] T.int),
        ( ">", TFun (Map.fromList [(TV "T", [Trait "Ord"])]) [TVar $ TV "T", TVar $ TV "T"] T.int),
        ("==", TFun (Map.fromList [(TV "T", [Trait "Eq"])]) [TVar $ TV "T", TVar $ TV "T"] T.int),
        ("!=", TFun (Map.fromList [(TV "T", [Trait "Eq"])]) [TVar $ TV "T", TVar $ TV "T"] T.int),
        ( ":", TFun (Map.fromList [(TV "T", []), (TV "U", [])]) [TVar $ TV "T", TVar $ TV "U"] (TVar $ TV "U"))
    ],
    un_ops = [
        ("!", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T"] T.int),
        ("-", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T"] (TVar $ TV "T"))
    ],
    fn_defs = [],
    vars = []
}
