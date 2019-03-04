{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Codegen.Codegen where
    
import Misc
import Annotation
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import Data.Maybe (fromJust)

import qualified Parser.Lib as L
import qualified Parser.Lang as P
import qualified Types as Ty
import qualified Codegen.Prelude as Pre
import qualified Codegen.Utils as U
import qualified Codegen.Specialization as Spe
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Operand as Op
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

codegenStmt :: P.TypedStmt -> U.CodegenTopLevel AST.Operand
codegenStmt = \case
    Ann (_, fn_ty) (P.Defn defn_ty name args ret_ty body) ->
        U.withScope $ mdo
            let final_name = case defn_ty of
                 P.Function -> name
                 P.Unary _ -> "unary_" <> name
                 P.Binary _ -> "binary_" <> name
            let ir_args = map (\(Ann (_, ty) (P.Arg name _)) -> (AST.mkName name, U.irType ty)) args
            let ir_type = ret_ty |> U.irType
            U.pushDeclAndImpl final_name ([] Ty.:=> fn_ty) (fn_ty, fn)
            blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ do
                _ <- Mn.block `Mn.named` "entry"
                op_args <- forM args $ \(Ann (_, ty) (P.Arg name _)) -> do
                    arg <- I.alloca (U.irType ty) Nothing 0
                    name <- Mn.fresh `Mn.named` toShort (pack name)
                    let ref = Op.LocalReference (U.irType ty) name
                    I.store arg 0 ref
                    return arg
                let names = map (\(Ann _ (P.Arg name _)) -> name) args
                let types = map (snd . annotation) args
                op_args |> zip3 names types
                        |> mapM_ (\(name, ty, arg) -> U.pushVar name ty arg)
                ret <- codegenExpr body
                I.ret ret
            fn <- U.function (AST.mkName final_name) ir_args ir_type blocks
            return fn
    Ann (_, ty) (P.Expr (Ann _ (P.Bin (Ann _ "=") (Ann _ (P.Ident name)) start))) -> do
        let ir_type = U.irType ty
        maybe_var <- U.getVar name
        case maybe_var of
            Nothing -> do
                var <- U.global (AST.mkName name) ir_type (U.defaultValue ty)
                U.pushVar name ty var
                blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ U.withScope $ do
                    _ <- Mn.block `Mn.named` "entry"
                    val <- codegenExpr start
                    I.store var 0 val
                    I.ret val
                name <- U.freshAnonName
                fn <- U.function (AST.mkName name) [] ir_type blocks
                U.pushExpr (ty, fn)
                return fn
            Just (_, var) -> do
                blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ U.withScope $ do
                    _ <- Mn.block `Mn.named` "entry"
                    val <- codegenExpr start
                    I.store var 0 val
                    I.ret val
                name <- U.freshAnonName
                fn <- U.function (AST.mkName name) [] ir_type blocks
                U.pushExpr (ty, fn)
                return fn
    Ann (_, ty) (P.Expr expr) -> do
        let ir_type = U.irType ty
        blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ U.withScope $ do
            _ <- Mn.block `Mn.named` "entry"
            ret <- codegenExpr expr
            I.ret ret
        name <- U.freshAnonName
        fn <- U.function (AST.mkName name) [] ir_type blocks
        U.pushExpr (ty, fn)
        return fn
    Ann (_, fn_ty) (P.Extern name args ret_ty) -> do
        let ir_args = map (\(Ann (_, ty) (P.Arg name _)) -> (AST.mkName name, U.irType ty)) args
        let ir_type = U.irType ret_ty
        fn <- U.extern (AST.mkName name) ir_args ir_type
        U.pushDeclAndImpl name ([] Ty.:=> fn_ty) (fn_ty, fn)
        return fn

codegenExpr ::P.TypedExpr -> U.Codegen AST.Operand
codegenExpr = \case
    Ann (_, ty) (P.For start cond oper body) ->
        U.withScope $ mdo
            _ <- codegenExpr start
            I.br for_cond_in
            entry <- Mn.currentBlock
            for_cond_in <- Mn.block `Mn.named` "for.cond"
            val <- I.phi [(AST.ConstantOperand $ U.defaultValue ty, entry), (body_ret, for_body_out)]
            cond_ret <- codegenExpr cond
            for_cond_out <- Mn.currentBlock
            inv_impl <- fmap fromJust $ U.getImpl "unary_!" ([snd (annotation cond)] Ty.:-> Ty.int)
            cond_inv <- I.call inv_impl [(cond_ret, [])]
            zero <- C.int64 0
            bool <- I.icmp IPred.EQ cond_inv zero
            I.condBr bool for_body_in for_end
            for_body_in <- Mn.block `Mn.named` "for.body"
            body_ret <- codegenExpr body
            _ <- codegenExpr oper
            for_body_out <- Mn.currentBlock
            I.br for_cond_in
            for_end <- Mn.block `Mn.named` "for.end"
            I.phi [(val, for_cond_out)]
    Ann (_, _) (P.If cond then_body else_block) ->
        U.withScope $ mdo
            cond_ret <- codegenExpr cond
            inv_impl <- fmap fromJust $ U.getImpl "unary_!" ([snd (annotation cond)] Ty.:-> Ty.int)
            cond_inv <- I.call inv_impl [(cond_ret, [])]
            zero <- C.int64 0
            bool <- I.icmp IPred.EQ cond_inv zero
            case else_block of
                Just else_body -> mdo
                    I.condBr bool if_then_in if_else_in
                    if_then_in <- Mn.block `Mn.named` "if.then"
                    ret_then <- codegenExpr then_body
                    if_then_out <- Mn.currentBlock
                    I.br if_end
                    if_else_in <- Mn.block `Mn.named` "if.else"
                    ret_else <- codegenExpr else_body
                    if_else_out <- Mn.currentBlock
                    I.br if_end
                    if_end <- Mn.block `Mn.named` "if.end"
                    I.phi [(ret_then, if_then_out), (ret_else, if_else_out)]
                Nothing -> mdo
                    I.condBr bool if_then_in if_end
                    if_then_in <- Mn.block `Mn.named` "if.then"
                    ret_then <- codegenExpr then_body
                    if_then_out <- Mn.currentBlock
                    I.br if_end
                    if_end <- Mn.block `Mn.named` "if.end"
                    I.phi [(ret_then, if_then_out)]
    Ann (_, ty) (P.While cond body) ->
        U.withScope $ mdo
            I.br while_cond_in
            entry <- Mn.currentBlock
            while_cond_in <- Mn.block `Mn.named` "while.cond"
            val <- I.phi [(AST.ConstantOperand $ U.defaultValue ty, entry), (body_ret, while_body_out)]
            cond_ret <- codegenExpr cond
            while_cond_out <- Mn.currentBlock
            inv_impl <- fmap fromJust $ U.getImpl "unary_!" ([snd (annotation cond)] Ty.:-> Ty.int)
            cond_inv <- I.call inv_impl [(cond_ret, [])]
            izero <- C.int64 0
            bool <- I.icmp IPred.EQ cond_inv izero
            I.condBr bool while_body_in while_end
            while_body_in <- Mn.block `Mn.named` "while.body"
            body_ret <- codegenExpr body
            while_body_out <- Mn.currentBlock
            I.br while_cond_in
            while_end <- Mn.block `Mn.named` "while.end"
            I.phi [(val, while_cond_out)]
    Ann (_, ty) (P.Call (Ann _ name) args) -> mdo
        let prototype = map (snd . annotation) args Ty.:-> ty
        maybe_decl <- U.getDecl name
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- args |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> do
                        impl <- U.Codegen $ lift $ specializeFunction prototype decl
                        gen_args <- args |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
            Nothing -> do
                var <- U.getVar name
                case var of
                    Just (fn_ty, ptr) | fn_ty == prototype -> do
                        gen_args <- args |> mapM codegenExpr
                        impl <- I.load ptr 0
                        gen_args |> map (, []) |> I.call impl
                    Just (fn_ty, _) -> error $ "Function " <> show name <> " has a wrong type (got: " <> show fn_ty <> ", expected: " <> show prototype <> ")"
                    Nothing -> error $ "Function " <> show name <> " not found: " <> show prototype
    Ann (_, ty) (P.Bin (Ann _ "=") (Ann _ (P.Ident name)) start) -> do
        maybe_var <- U.getVar name
        case maybe_var of
            Just (_, var_op) -> do
                val <- codegenExpr start
                I.store var_op 0 val
                return val
            Nothing -> do
                val <- codegenExpr start
                var <- I.alloca (U.irType ty) Nothing 0
                U.pushVar name ty var
                I.store var 0 val
                return val
    Ann (_, ty) (P.Bin (Ann _ name) lhs rhs) -> mdo
        let prefixed = "binary_" <> name
        maybe_decl <- U.getDecl prefixed
        let prototype = map (snd . annotation) [lhs, rhs] Ty.:-> ty
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- [lhs, rhs] |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> do
                        impl <- U.Codegen $ lift $ specializeFunction prototype decl
                        gen_args <- [lhs, rhs] |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
            Nothing -> error $ "Function " <> show prefixed <> " not found: " <> show prototype
    Ann (_, ty) (P.Un (Ann _ name) rhs) -> mdo
        let prefixed = "unary_" <> name
        maybe_decl <- U.getDecl prefixed
        let prototype = map (snd . annotation) [rhs] Ty.:-> ty
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- [rhs] |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> do
                        impl <- U.Codegen $ lift $ specializeFunction prototype decl
                        gen_args <- [rhs] |> mapM codegenExpr
                        gen_args |> map (, []) |> I.call impl
            Nothing -> error $ "Function " <> show prefixed <> " not found: " <> show prototype
    Ann (_, ty) (P.Ident name) -> mdo
        found <- U.getVar name
        case found of
            Just (_, op) -> I.load op 0
            Nothing -> do
                maybe_decl <- U.getDecl name
                case maybe_decl of
                    Just decl ->
                        case Map.lookup ty $ U.impls decl of
                            Just impl -> return impl
                            Nothing -> U.Codegen $ lift $ specializeFunction ty decl
                    Nothing -> error $ "Variable " <> show name <> " not found"
    Ann (_, ty) (P.Lambda args body) ->
        U.withScope $ U.Codegen $ lift $ do
            let ir_args = map (\(Ann (_, ty) name) -> (AST.mkName name, U.irType ty)) args
            let ir_type = U.irType ((\(_ Ty.:-> ret) -> ret) ty)
            blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ do
                _ <- Mn.block `Mn.named` "entry"
                op_args <- forM args $ \(Ann (_, ty) name) -> do
                    arg <- I.alloca (U.irType ty) Nothing 0
                    name <- Mn.fresh `Mn.named` toShort (pack name)
                    let ref = Op.LocalReference (U.irType ty) name
                    I.store arg 0 ref
                    return arg
                let names = map (\(Ann _ name) -> name) args
                let types = map (snd . annotation) args
                op_args |> zip3 names types
                        |> mapM_ (\(name, ty, arg) -> U.pushVar name ty arg)
                ret <- codegenExpr body
                I.ret ret
            name <- U.freshLambdaName
            U.function (AST.mkName name) ir_args ir_type blocks
    Ann (_, Ty.TCon (Ty.TC "integer")) (P.Lit (P.IntLiteral i)) -> C.int64 $ fromIntegral i
    Ann (_, Ty.TCon (Ty.TC "double")) (P.Lit (P.IntLiteral i)) -> C.double $ fromIntegral i
    Ann (_, _) (P.Lit (P.DoubleLiteral d)) -> C.double d
    Ann (_, _) (P.Lit (P.BooleanLiteral b)) -> C.bit (if b then 1 else 0)
    Ann (_, _) (P.Lit P.VoidLiteral) -> error "Void literal"
    Ann _ node -> error ("Codegen: Node not supported (" <> show node <> ")")

specializeFunction :: Ty.Type -> U.FnDecl -> U.CodegenTopLevel AST.Operand
specializeFunction
    fn_ty@(t_args Ty.:-> t_ret_ty)
    U.FnDecl { body = Just (P.Defn _ name args _ body) } = mdo
        let arg_types = map (\(Ann (_, ty) _) -> ty) args
        let varMap = flip execState Map.empty $ forM (zip t_args arg_types) $ \(t, a) ->
             case a of
                 Ty.TVar v -> modify (Map.insert v t)
                 _ -> return ()
        let specialized_body = evalState (Spe.specializeExpr body) varMap
        let final_name = U.mangleFunction name t_args t_ret_ty
        let ir_args = map (\(Ann _ (P.Arg name _), ty) -> (AST.mkName name, U.irType ty)) (zip args t_args)
        let ir_type = t_ret_ty |> U.irType
        U.pushImpl name fn_ty fn
        blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ U.runCodegen $ do
            _ <- Mn.block `Mn.named` "entry"
            op_args <- forM (zip args t_args) $ \(Ann _ (P.Arg name _), ty) -> do
                arg <- I.alloca (U.irType ty) Nothing 0
                name <- Mn.fresh `Mn.named` toShort (pack name)
                let ref = Op.LocalReference (U.irType ty) name
                I.store arg 0 ref
                return arg
            let names = map (\(Ann _ (P.Arg name _)) -> name) args
            let types = map (snd . annotation) args
            op_args |> zip3 names types
                    |> mapM_ (\(name, ty, arg) -> U.pushVar name ty arg)
            ret <- codegenExpr specialized_body
            I.ret ret
        fn <- U.function (AST.mkName final_name) ir_args ir_type blocks
        return fn
specializeFunction a b = error $ "specializeFunction:\n" <> show a <> "\n" <> show b

codegenAST :: P.AST (L.Range, Ty.Type) -> AST.Module
codegenAST stmts =
    let topLevel = do
            printf <- U.externVarArgs (AST.mkName "printf") [(AST.mkName "fmt", T.ptr T.i8)] T.i32
            doubleFmt <- U.preludeStringPtr "%lf\n" "PRINT_DOUBLE"
            intFmt <- U.preludeStringPtr "%ld\n" "PRINT_INT"
            strFmt <- U.preludeStringPtr "%s\n" "PRINT_STR"
            trueStr <- U.preludeStringPtr "true" "TRUE_STR"
            falseStr <- U.preludeStringPtr "false" "FALSE_STR"
            let printGenericProto = [("T", ["Show"])] Ty.:=> ["a"] Ty.:-> Ty.void
            U.pushDecl "print" printGenericProto Nothing
            printDouble <- U.preludeFunction (AST.mkName "print_double") [(U.double, M.ParameterName "n")] U.void $ \[n] -> mdo
                _ <- Mn.block `Mn.named` "entry"
                _ <- I.call printf [(doubleFmt, []), (n, [])]
                return ()
            U.pushImpl "print" ([Ty.double] Ty.:-> Ty.void) printDouble
            printInt <- U.preludeFunction (AST.mkName "print_int") [(U.int, M.ParameterName "n")] U.void $ \[n] -> mdo
                _ <- Mn.block `Mn.named` "entry"
                _ <- I.call printf [(intFmt, []), (n, [])]
                return ()
            U.pushImpl "print" ([Ty.int] Ty.:-> Ty.void) printInt
            printBool <- U.preludeFunction (AST.mkName "print_bool") [(U.bool, M.ParameterName "n")] U.void $ \[n] -> mdo
                _ <- Mn.block `Mn.named` "entry"
                ret <- I.select n trueStr falseStr
                _ <- I.call printf [(strFmt, []), (ret, [])]
                return ()
            U.pushImpl "print" ([Ty.bool] Ty.:-> Ty.void) printBool
            Pre.prelude
            mapM_ codegenStmt stmts
            exps <- gets $ \env -> env |> U.exprs |> reverse
            blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
                _ <- Mn.block `Mn.named` "entry"
                forM_ exps $ \(ty, expr) -> do
                    ret <- I.call expr []
                    let prototype = [ty] Ty.:-> Ty.void
                    maybe_decl <- U.getImpl "print" prototype
                    case maybe_decl of
                        Nothing -> return () -- error $ "Function \"print\" not found: " <> show prototype
                        Just impl -> void $ I.call impl [(ret, [])]
                ret <- C.int64 0
                I.ret ret
            U.function (AST.mkName "main") [] T.i64 blocks
        definitions = topLevel |> U.runCodegenTopLevel
                               |> M.execModuleBuilderT M.emptyModuleBuilder
                               |> flip evalState defaultEnv
    in AST.Module {
        AST.moduleName = "KOAK",
        AST.moduleSourceFileName = "input.koak",
        AST.moduleDataLayout = Nothing,
        AST.moduleTargetTriple = Nothing,
        AST.moduleDefinitions = definitions
    }

defaultEnv :: U.Env
defaultEnv = U.Env {
    U.vars = [Map.empty],
    U.decls = Map.empty,
    U.exprs = [],
    U.anon_count = 0,
    U.lambda_count = 0
}
