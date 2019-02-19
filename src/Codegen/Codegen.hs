{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen.Codegen where
    
import Misc
import Annotation
import Control.Monad.State.Lazy
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import Data.List (partition)
import Data.Maybe (fromJust)

import qualified Parser.Lang as P
import qualified Types as Ty
import qualified Codegen.Prelude as Pre
import qualified Codegen.Utils as U
import qualified Codegen.Specialization as Spe
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Operand as Op
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

instance U.Codegen P.Literal where
    codegen a@(P.IntLiteral i) = C.int64 $ fromIntegral i
    codegen a@(P.DoubleLiteral d) = C.double d
    codegen a@P.VoidLiteral = error "codegen VoidLiteral not implemented"

instance U.CodegenTopLevel (P.Stmt Ty.Type) where
    codegenTopLevel (P.Defn fn_ty defnTy name args ret_ty body) = mdo
        let final_name = case defnTy of
             P.Function -> name
             P.Unary -> "unary_" ++ name
             P.Binary -> "binary_" ++ name
        let ir_args = map (\(P.Arg ty name _) -> (AST.mkName name, U.irType ty)) args
        let ir_type = ret_ty |> U.irType
        lift $ U.pushDeclAndImpl final_name fn_ty (fn_ty, fn)
        blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            entry <- Mn.block `Mn.named` "entry"
            op_args <- forM args $ \(P.Arg ty name _) -> do
                arg <- I.alloca (U.irType ty) Nothing 0
                name <- Mn.fresh `Mn.named` toShort (pack name)
                let ref = Op.LocalReference (U.irType ty) name
                I.store arg 0 ref
                return arg
            let names = map (\(P.Arg _ name _) -> name) args
            let types = map P.getArgAnn args
            lift $ lift (op_args |> zip3 names types
                                 |> mapM (\(name, ty, arg) -> U.pushVar name ty arg))
            ret <- U.codegen body
            I.ret ret
        fn <- U.function (AST.mkName final_name) ir_args ir_type blocks
        return fn
    codegenTopLevel (P.Expr ty (P.Bin _ (Ann _ "=") (P.Ident _ name) start)) = do
        let ir_type = U.irType ty
        maybe_var <- lift $ U.getVar name
        case maybe_var of
            Nothing -> do
                var <- M.global (AST.mkName name) ir_type (U.defaultValue ty)
                lift $ U.pushVar name ty var
                blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
                    entry <- Mn.block `Mn.named` "entry"
                    lift $ lift U.newScope
                    val <- U.codegen start
                    I.store var 0 val
                    lift $ lift U.dropScope
                    I.ret val
                count <- lift $ gets $ \env -> env |> U.exprs |> length
                fn <- U.function (AST.mkName ("__anon_" ++ show count)) [] ir_type blocks
                lift $ U.pushExpr (ty, fn)
                return fn
            Just (_, var) -> do
                blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
                    entry <- Mn.block `Mn.named` "entry"
                    lift $ lift U.newScope
                    val <- U.codegen start
                    I.store var 0 val
                    lift $ lift U.dropScope
                    I.ret val
                count <- lift $ gets $ \env -> env |> U.exprs |> length
                fn <- U.function (AST.mkName ("__anon_" ++ show count)) [] ir_type blocks
                lift $ U.pushExpr (ty, fn)
                return fn
    codegenTopLevel (P.Expr ty expr) = do
        let ir_type = U.irType ty
        blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            lift $ lift U.newScope
            entry <- Mn.block `Mn.named` "entry"
            ret <- U.codegen expr
            lift $ lift U.dropScope
            I.ret ret
        count <- lift $ gets $ \env -> env |> U.exprs |> length
        fn <- U.function (AST.mkName ("__anon_" ++ show count)) [] ir_type blocks
        lift $ U.pushExpr (ty, fn)
        return fn
    codegenTopLevel (P.Extern fn_ty name args ret_ty) = do
        let ir_args = map (\(P.Arg ty name _) -> (AST.mkName name, U.irType ty)) args
        let ir_type = U.irType ret_ty
        fn <- U.extern (AST.mkName name) ir_args ir_type
        lift $ U.pushDeclAndImpl name fn_ty (fn_ty, fn)
        return fn

instance U.Codegen (P.Expr Ty.Type) where
    codegen (P.For ty start cond oper body) = mdo
        lift $ lift U.newScope
        start_ret <- U.codegen start
        I.br for_cond_in
        entry <- Mn.currentBlock
        for_cond_in <- Mn.block `Mn.named` "for.cond"
        val <- I.phi [(AST.ConstantOperand $ U.defaultValue ty, entry), (body_ret, for_body_out)]
        cond_ret <- U.codegen cond
        for_cond_out <- Mn.currentBlock
        inv_impl <- lift $ lift $ fmap fromJust $ U.getImpl "unary_!" $ Ty.TFun Map.empty [P.getExprAnn cond] Ty.int
        cond_inv <- I.call inv_impl [(cond_ret, [])]
        zero <- C.int64 0
        bool <- I.icmp IPred.EQ cond_inv zero
        I.condBr bool for_body_in for_end
        for_body_in <- Mn.block `Mn.named` "for.body"
        body_ret <- U.codegen body
        oper_ret <- U.codegen oper
        for_body_out <- Mn.currentBlock
        I.br for_cond_in
        for_end <- Mn.block `Mn.named` "for.end"
        lift $ lift U.dropScope
        I.phi [(val, for_cond_out)]
    codegen (P.If ty cond then_body else_block) = mdo
        lift $ lift U.newScope
        cond_ret <- U.codegen cond
        inv_impl <- lift $ lift $ fmap fromJust $ U.getImpl "unary_!" $ Ty.TFun Map.empty [P.getExprAnn cond] Ty.int
        cond_inv <- I.call inv_impl [(cond_ret, [])]
        zero <- C.int64 0
        bool <- I.icmp IPred.EQ cond_inv zero
        case else_block of
            Just else_body -> mdo
                I.condBr bool if_then_in if_else_in
                if_then_in <- Mn.block `Mn.named` "if.then"
                ret_then <- U.codegen then_body
                if_then_out <- Mn.currentBlock
                I.br if_end
                if_else_in <- Mn.block `Mn.named` "if.else"
                ret_else <- U.codegen else_body
                if_else_out <- Mn.currentBlock
                I.br if_end
                if_end <- Mn.block `Mn.named` "if.end"
                lift $ lift U.dropScope
                I.phi [(ret_then, if_then_out), (ret_else, if_else_out)]
            Nothing -> mdo
                I.condBr bool if_then_in if_end
                if_then_in <- Mn.block `Mn.named` "if.then"
                ret_then <- U.codegen then_body
                if_then_out <- Mn.currentBlock
                I.br if_end
                if_end <- Mn.block `Mn.named` "if.end"
                lift $ lift U.dropScope
                I.phi [(ret_then, if_then_out)]
    codegen (P.While ty cond body) = mdo
        lift $ lift U.newScope
        I.br while_cond_in
        entry <- Mn.currentBlock
        while_cond_in <- Mn.block `Mn.named` "while.cond"
        val <- I.phi [(AST.ConstantOperand $ U.defaultValue ty, entry), (body_ret, while_body_out)]
        cond_ret <- U.codegen cond
        while_cond_out <- Mn.currentBlock
        inv_impl <- lift $ lift $ fmap fromJust $ U.getImpl "unary_!" $ Ty.TFun Map.empty [P.getExprAnn cond] Ty.int
        cond_inv <- I.call inv_impl [(cond_ret, [])]
        izero <- C.int64 0
        bool <- I.icmp IPred.EQ cond_inv izero
        I.condBr bool while_body_in while_end
        while_body_in <- Mn.block `Mn.named` "while.body"
        body_ret <- U.codegen body
        while_body_out <- Mn.currentBlock
        I.br while_cond_in
        while_end <- Mn.block `Mn.named` "while.end"
        lift $ lift U.dropScope
        I.phi [(val, while_cond_out)]
    codegen (P.Call ty (Ann _ name) args) = mdo
        maybe_decl <- lift $ lift $ U.getDecl name
        let prototype = Ty.TFun Map.empty (map P.getExprAnn args) ty
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- args |> mapM U.codegen
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> case U.body decl of
                        Just body -> do
                            impl <- lift $ specializeFunction prototype decl
                            gen_args <- args |> mapM U.codegen
                            gen_args |> map (, []) |> I.call impl
                        Nothing -> error $ "Can't specialize function " ++ show name ++ ": " ++ show prototype
            Nothing -> error $ "Function " ++ show name ++ " not found: " ++ show prototype
    codegen (P.Bin ty (Ann _ "=") (P.Ident _ name) start) = do
        maybe_var <- lift $ lift $ U.getVar name
        case maybe_var of
            Just (_, var_op) -> do
                val <- U.codegen start
                I.store var_op 0 val
                return val
            Nothing -> do
                val <- U.codegen start
                var <- I.alloca (U.irType ty) Nothing 0
                lift $ lift $ U.pushVar name ty var
                I.store var 0 val
                return val
    codegen (P.Bin ty (Ann _ name) lhs rhs) = mdo
        let prefixed = "binary_" ++ name
        maybe_decl <- lift $ lift $ U.getDecl prefixed
        let prototype = Ty.TFun Map.empty (map P.getExprAnn [lhs, rhs]) ty
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- [lhs, rhs] |> mapM U.codegen
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> case U.body decl of
                        Just body -> do
                            impl <- lift $ specializeFunction prototype decl
                            gen_args <- [lhs, rhs] |> mapM U.codegen
                            gen_args |> map (, []) |> I.call impl
                        Nothing -> error $ "Can't specialize function " ++ show prefixed ++ ": " ++ show prototype
            Nothing -> error $ "Function " ++ show prefixed ++ " not found: " ++ show prototype
    codegen (P.Un ty (Ann _ name) rhs) = mdo
        let prefixed = "unary_" ++ name
        maybe_decl <- lift $ lift $ U.getDecl prefixed
        let prototype = Ty.TFun Map.empty (map P.getExprAnn [rhs]) ty
        case maybe_decl of
            Just decl ->
                case Map.lookup prototype $ U.impls decl of
                    Just impl -> do
                        gen_args <- [rhs] |> mapM U.codegen
                        gen_args |> map (, []) |> I.call impl
                    Nothing -> case U.body decl of
                        Just body -> do
                            impl <- lift $ specializeFunction prototype decl
                            gen_args <- [rhs] |> mapM U.codegen
                            gen_args |> map (, []) |> I.call impl
                        Nothing -> error $ "Can't specialize function " ++ show prefixed ++ ": " ++ show prototype
            Nothing -> error $ "Function " ++ show prefixed ++ " not found: " ++ show prototype
    codegen (P.Ident _ name) = mdo
        found <- lift $ lift $ U.getVar name
        maybe
            (do env <- get
                error $ "Variable " ++ show name ++ " not found: " ++ show env)
            (\(_, op) -> I.load op 0)
            found
    codegen (P.Lit ty lit@(P.IntLiteral i)) = C.int64 $ fromIntegral i
    codegen (P.Lit ty lit@(P.DoubleLiteral d)) = C.double d
    codegen (P.Lit ty lit@(P.BooleanLiteral b)) = C.bit (if b then 1 else 0)
    codegen (P.Lit ty lit@P.VoidLiteral) = error "Void literal"

specializeFunction :: Ty.Type -> U.FnDecl -> M.ModuleBuilderT (State U.Env) AST.Operand
specializeFunction
    fn_ty@(Ty.TFun _ t_args t_ret_ty)
    U.FnDecl { body = Just (P.Defn _ defnTy name args ret_ty body) } = mdo
        let arg_types = map (\(P.Arg _ _ ty) -> ty) args
        let varMap = flip execState Map.empty $ forM (zip t_args arg_types) $ \(t, a) ->
             case a of
                 Ty.TVar v -> modify (Map.insert v t)
                 _ -> return ()
        let specialized_body = evalState (Spe.specialize body) varMap
        let final_name = U.mangleFunction name t_args t_ret_ty
        let ir_args = map (\(P.Arg _ name _, ty) -> (AST.mkName name, U.irType ty)) (zip args t_args)
        let ir_type = t_ret_ty |> U.irType
        lift $ U.pushImpl name fn_ty fn
        blocks <- Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            entry <- Mn.block `Mn.named` "entry"
            op_args <- forM (zip args t_args) $ \(P.Arg _ name _, ty) -> do
                arg <- I.alloca (U.irType ty) Nothing 0
                name <- Mn.fresh `Mn.named` toShort (pack name)
                let ref = Op.LocalReference (U.irType ty) name
                I.store arg 0 ref
                return arg
            let names = map (\(P.Arg _ name _) -> name) args
            let types = map P.getArgAnn args
            lift $ lift (op_args |> zip3 names types
                                 |> mapM (\(name, ty, arg) -> U.pushVar name ty arg))
            ret <- U.codegen specialized_body
            I.ret ret
        fn <- U.function (AST.mkName final_name) ir_args ir_type blocks
        return fn

codegenAST :: P.AST Ty.Type -> AST.Module
codegenAST stmts =
    let topLevel = do
            printf <- U.externVarArgs (AST.mkName "printf") [(AST.mkName "fmt", T.ptr T.i8)] T.i32
            doubleFmt <- U.stringPtr "%lf\n" "PRINT_DOUBLE"
            intFmt <- U.stringPtr "%ld\n" "PRINT_INT"
            voidFmt <- U.stringPtr "()\n" "PRINT_VOID"
            strFmt <- U.stringPtr "%s\n" "PRINT_STR"
            trueStr <- U.stringPtr "true" "TRUE_STR"
            falseStr <- U.stringPtr "false" "FALSE_STR"
            let printGenericProto = Ty.TFun (Map.fromList [(Ty.TV "a", [Ty.Trait "Show"])]) [Ty.TVar $ Ty.TV "a"] Ty.void
            lift $ U.pushDecl "print" printGenericProto Nothing
            printDouble <- M.function (AST.mkName "print_double") [(U.double, M.ParameterName "n")] U.void $ \[n] -> mdo
                entry <- Mn.block `Mn.named` "entry"
                I.call printf [(doubleFmt, []), (n, [])]
                return ()
            lift $ U.pushImpl "print" (Ty.TFun Map.empty [Ty.double] Ty.void) printDouble
            printInt <- M.function (AST.mkName "print_int") [(U.int, M.ParameterName "n")] U.void $ \[n] -> mdo
                entry <- Mn.block `Mn.named` "entry"
                I.call printf [(intFmt, []), (n, [])]
                return ()
            lift $ U.pushImpl "print" (Ty.TFun Map.empty [Ty.int] Ty.void) printInt
            printBool <- M.function (AST.mkName "print_bool") [(U.bool, M.ParameterName "n")] U.void $ \[n] -> mdo
                entry <- Mn.block `Mn.named` "entry"
                ret <- I.select n trueStr falseStr
                I.call printf [(strFmt, []), (ret, [])]
                return ()
            lift $ U.pushImpl "print" (Ty.TFun Map.empty [Ty.bool] Ty.void) printBool
            Pre.prelude
            mapM_ U.codegenTopLevel stmts
            exps <- lift $ gets $ \env -> env |> U.exprs |> reverse
            blocks <- lift $ Mn.execIRBuilderT Mn.emptyIRBuilder $ do
                entry <- Mn.block `Mn.named` "entry"
                forM_ exps $ \(ty, exp) -> do
                    ret <- I.call exp []
                    let prototype = Ty.TFun Map.empty [ty] Ty.void
                    maybe_decl <- lift $ U.getImpl "print" prototype
                    maybe
                        (error $ "Function \"print\" not found: " ++ show prototype)
                        (\impl -> I.call impl [(ret, [])])
                        maybe_decl
                ret <- C.int64 0
                I.ret ret
            U.function (AST.mkName "main") [] T.i64 blocks
        definitions = topLevel |> M.execModuleBuilderT M.emptyModuleBuilder |> flip evalState defaultEnv
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
    U.exprs = []
}
