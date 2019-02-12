{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
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
data Env = Env {
    bin_ops :: Scope,
    un_ops :: Scope,
    fn_defs :: Scope,
    vars :: [Scope]
} deriving (Show, Eq)
type Annotated = ExceptT Error (State Env)

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

class Annotate a where
    annotate :: a -> Annotated (Ann Type a)

class Annotate1 a where
    annotate1 :: a b -> Annotated (Ann Type (a Type))

instance Annotate P.Type where
    annotate a@P.IntType = return $ Ann T.int a
    annotate a@P.FloatType = return $ Ann T.float a
    annotate a@P.VoidType = return $ Ann T.void a

instance Annotate P.Literal where
    annotate a@(P.IntLiteral _) = return $ Ann T.int a
    annotate a@(P.FloatLiteral _) = return $ Ann T.float a
    annotate a@P.VoidLiteral = return $ Ann T.void a

instance Annotate P.Arg where
    annotate arg@P.Arg{ P.arg_name = name, P.arg_type = arg_ty } = do
        annotated_arg <- annotate arg_ty
        let ty = annotation annotated_arg
        lift $ pushVar name ty
        return $ Ann ty arg

instance Annotate1 P.Stmt where
    annotate1 (P.DefnStmt (Ann _ defn)) = do
        annotated_defn <- annotate1 defn
        let ty = annotation annotated_defn
        return $ Ann ty $ P.DefnStmt annotated_defn
    annotate1 (P.ExprStmt (Ann _ expr)) = do
        annotated_expr <- annotate1 expr
        let ty = annotation annotated_expr
        return $ Ann ty $ P.ExprStmt annotated_expr

instance Annotate1 P.Defn where
    annotate1 (P.Op (Ann _ defn)) = do
        annotated_defn <- annotate1 defn
        let ty = annotation annotated_defn
        return $ Ann ty $ P.Op annotated_defn
    annotate1 (P.Fn (Ann _ defn)) = do
        annotated_defn <- annotate1 defn
        let ty = annotation annotated_defn
        return $ Ann ty $ P.Fn annotated_defn

instance Annotate1 P.OpDefn where
    annotate1 defn@P.OpDefn {
        P.opdefn_op = op,
        P.opdefn_arity = arity,
        P.opdefn_args = args,
        P.opdefn_ret_ty = ret_ty,
        P.opdefn_body = Ann _ body
    } = do
        lift newScope
        annotated_args <- sequence $ map (\(Ann _ a) -> annotate a) args
        let tys = map annotation annotated_args
        Ann expected _ <- annotate ret_ty
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
                    Nothing -> lift $ pushBinOp op ty
        annotated_body <- annotate1 body
        let inferred = annotation annotated_body
        lift dropScope
        if inferred == expected
            then return $ Ann ty $ defn {
                P.opdefn_args = annotated_args,
                P.opdefn_body = annotated_body
            }
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Annotate1 P.FnDefn where
    annotate1 defn@P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = Ann _ body
    } = do
        lift newScope
        annotated_args <- sequence $ map (\(Ann _ a) -> annotate a) args
        let tys = map annotation annotated_args
        Ann expected _ <- annotate ret_ty
        let ty = TFun Map.empty tys expected
        maybe_fn <- lift $ getFnDef name
        case maybe_fn of
            Just ty2 -> throwE $ MultipleDefnErr $ MultipleDefnError {
                name, definitions = [ty2, ty]
            }
            Nothing -> lift $ pushFnDef name ty
        annotated_body <- annotate1 body
        let inferred = annotation annotated_body
        lift dropScope
        if inferred == expected
            then return $ Ann ty $ defn {
                P.fndefn_args = annotated_args,
                P.fndefn_body = annotated_body
            }
            else throwE $ TypeErr $ TypeError {
                expected = expected,
                got = inferred
            }

instance Annotate1 P.ForExpr where
    annotate1 P.ForExpr {
        P.for_init = Ann _ init,
        P.for_cond = Ann _ cond,
        P.for_oper = Ann _ oper,
        P.for_body = Ann _ body
    } = do
        lift newScope
        tys <- sequence $ map annotate1 [init, cond, oper]
        annotated_body <- annotate1 body
        let ty = annotation annotated_body
        lift dropScope
        return $ Ann ty $ P.ForExpr {
            P.for_init = tys !! 0,
            P.for_cond = tys !! 1,
            P.for_oper = tys !! 2,
            P.for_body = annotated_body
        }

instance Annotate1 P.IfExpr where
    annotate1 P.IfExpr {
        P.if_cond = Ann _ cond,
        P.if_then = Ann _ body,
        P.if_else = else_block
    } = do
        lift newScope
        annotated_cond <- annotate1 cond
        annotated_then <- annotate1 body
        let then_ty = annotation annotated_then
        maybe (do 
            lift dropScope
            return $ Ann then_ty $ P.IfExpr {
                P.if_cond = annotated_cond,
                P.if_then = annotated_then,
                P.if_else = Nothing
            })
            (\(Ann _ block) -> do
                annotated_else <- annotate1 block
                let else_ty = annotation annotated_else
                lift dropScope
                if then_ty == else_ty
                    then return $ Ann then_ty $ P.IfExpr {
                        P.if_cond = annotated_cond,
                        P.if_then = annotated_then,
                        P.if_else = Just annotated_else
                    }
                    else throwE $ TypeErr $ TypeError {
                        expected = then_ty,
                        got = else_ty
                    })
            else_block

instance Annotate1 P.WhileExpr where
    annotate1 P.WhileExpr {
        P.while_cond = (Ann _ cond),
        P.while_body = (Ann _ body)
    } = do
        lift newScope
        annotated_cond <- annotate1 cond
        annotated_body <- annotate1 body
        let body_ty = annotation annotated_body
        lift dropScope
        return $ Ann body_ty $ P.WhileExpr {
            P.while_cond = annotated_cond,
            P.while_body = annotated_body
        }

instance Annotate1 P.CallExpr where
    annotate1 P.CallExpr {
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
        annotated_args <- sequence $ map (\(Ann _ a) -> annotate1 a) args
        let args_tys = map annotation annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ Ann ty $ P.CallExpr {
                P.call_ident = name,
                P.call_args = annotated_args
            }
            Left err -> throwE err

instance Annotate1 P.BinExpr where
    -- TODO: Check if ${lhs} already exists (in current scope only).
    annotate1 P.BinExpr {
        P.bin_op = "=",
        P.bin_lhs = Ann _ lhs,
        P.bin_rhs = Ann _ rhs
    } = do
        annotated_rhs <- annotate1 rhs
        let ty = annotation annotated_rhs
        case lhs of
            P.Ident (Ann _ name) -> do
                lift $ pushVar name ty
                return $ Ann ty $ P.BinExpr {
                    P.bin_op = "=",
                    P.bin_lhs = Ann ty $ P.Ident $ Ann ty name,
                    P.bin_rhs = annotated_rhs
                }
            _ -> throwE $ AssignErr AssignError
    annotate1 P.BinExpr {
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
        annotated_args <- sequence $ map annotate1 [lhs, rhs]
        let args_tys = map annotation annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ Ann ty $ P.BinExpr {
                P.bin_op = name,
                P.bin_lhs = annotated_args !! 0,
                P.bin_rhs = annotated_args !! 1
            }
            Left err -> throwE err

instance Annotate1 P.UnExpr where
    annotate1 P.UnExpr {
        P.un_op = name,
        P.un_rhs = Ann _ rhs
    } = do
        fun_ty <- do
            found <- lift $ gets $ \Env { un_ops } ->
                Map.lookup name un_ops
            maybe
                (throwE $ NotInScopeErr $ NotInScopeError { ident = name })
                return
                found
        annotated_args <- sequence $ map annotate1 [rhs]
        let args_tys = map annotation annotated_args
        case apply fun_ty args_tys of
            Right ty -> return $ Ann ty $ P.UnExpr {
                P.un_op = name,
                P.un_rhs = annotated_args !! 0
            }
            Left err -> throwE err

instance Annotate1 P.Expr where
    annotate1 (P.For (Ann _ forExpr)) = do
        annotated_for <- annotate1 forExpr
        let ty = annotation annotated_for
        return $ Ann ty $ P.For annotated_for
    annotate1 (P.If (Ann _ ifExpr)) = do
        annotated_if <- annotate1 ifExpr
        let ty = annotation annotated_if
        return $ Ann ty $ P.If annotated_if
    annotate1 (P.While (Ann _ whileExpr)) = do
        annotated_while <- annotate1 whileExpr
        let ty = annotation annotated_while
        return $ Ann ty $ P.While annotated_while
    annotate1 (P.Ident (Ann _ ident)) = do
        found <- lift $ gets $ \Env { bin_ops, un_ops, fn_defs, vars } ->
            foldl (<|>) Nothing $ fmap (Map.lookup ident) (vars ++ [fn_defs, un_ops, bin_ops])
        maybe
            (throwE $ NotInScopeErr $ NotInScopeError { ident })
            (\ty -> return $ Ann ty $ P.Ident $ Ann ty ident)
            found
    annotate1 (P.Lit (Ann _ literal)) = do
        annotated_literal <- annotate literal
        let ty = annotation annotated_literal
        return $ Ann ty $ P.Lit annotated_literal
    annotate1 (P.Call (Ann _ callExpr)) = do
        annotated_call <- annotate1 callExpr
        let ty = annotation annotated_call
        return $ Ann ty $ P.Call annotated_call
    annotate1 (P.Un (Ann _ unExpr)) = do
        annotated_unexpr <- annotate1 unExpr
        let ty = annotation annotated_unexpr
        return $ Ann ty $ P.Un annotated_unexpr
    annotate1 (P.Bin (Ann _ binExpr)) = do
        annotated_binexpr <- annotate1 binExpr
        let ty = annotation annotated_binexpr
        return $ Ann ty $ P.Bin annotated_binexpr

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
    stmts |> map (\(Ann _ stmt) -> annotate1 stmt)
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
