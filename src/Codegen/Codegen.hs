{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen.Codegen where
    
import Misc
import Annotation
import Control.Monad.State.Lazy
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import Data.Char (ord)
import Data.List (partition)
import Data.Maybe (fromJust)

import qualified Parser.Lang as P
import qualified Types as Ty
import qualified Codegen.Prelude as Pre
import qualified Codegen.Utils as U
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Operand as Op
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

convertType :: P.Type -> Ty.Type
convertType ty | ty == P.IntType = Ty.int
convertType ty | ty == P.FloatType = Ty.float
convertType ty | ty == P.VoidType = Ty.void

getPrintInt, getPrintDouble :: State U.Env AST.Operand
getPrintInt = fmap fromJust $ U.getImpl "printInt" $ Ty.TFun Map.empty [Ty.int] Ty.void
getPrintDouble = fmap fromJust $ U.getImpl "printDouble" $ Ty.TFun Map.empty [Ty.float] Ty.void

prelude :: M.ModuleBuilderT (State U.Env) ()
prelude = do
    addI <- Pre.addInt
    addD <- Pre.addDouble
    subI <- Pre.subInt
    subD <- Pre.subDouble
    multI <- Pre.multInt
    multD <- Pre.multDouble
    divI <- Pre.divInt
    divD <- Pre.divDouble
    let ty1 = Ty.TFun (Map.fromList [(Ty.TV "T", [Ty.Trait "Num"])]) [Ty.TVar $ Ty.TV "T", Ty.TVar $ Ty.TV "T"] (Ty.TVar $ Ty.TV "T")
    lift $ U.pushDecl "+" ty1
    lift $ U.pushDecl "-" ty1
    lift $ U.pushDecl "*" ty1
    lift $ U.pushDecl "/" ty1
    lift $ U.pushImpl "+" (Ty.TFun Map.empty [Ty.int, Ty.int] Ty.int) addI
    lift $ U.pushImpl "+" (Ty.TFun Map.empty [Ty.float, Ty.float] Ty.float) addD
    lift $ U.pushImpl "-" (Ty.TFun Map.empty [Ty.int, Ty.int] Ty.int) subI
    lift $ U.pushImpl "-" (Ty.TFun Map.empty [Ty.float, Ty.float] Ty.float) subD
    lift $ U.pushImpl "*" (Ty.TFun Map.empty [Ty.int, Ty.int] Ty.int) multI
    lift $ U.pushImpl "*" (Ty.TFun Map.empty [Ty.float, Ty.float] Ty.float) multD
    lift $ U.pushImpl "/" (Ty.TFun Map.empty [Ty.int, Ty.int] Ty.int) divI
    lift $ U.pushImpl "/" (Ty.TFun Map.empty [Ty.float, Ty.float] Ty.float) divD

-- fib :: AST.Module
-- fib = M.buildModule "koak" $ mdo
--     intFmt <- U.stringPtr "%d\n" "PRINT_INT"
--     printf <- U.externVarArgs (AST.mkName "printf") [(AST.mkName "fmt", T.ptr T.i8)] T.i32
--     fib <- M.function (AST.mkName "fibonnacci") [(T.i32, M.ParameterName "n")] T.i32 $ \[n] -> mdo
--         entry <- Mn.block `Mn.named` "entry"
--         I.ret n
--     M.function (AST.mkName "main") [] T.i32 $ \[] -> mdo
--         entry <- Mn.block `Mn.named` "entry"
--         arg <- C.int32 5
--         fibVal <- I.call fib [(arg, [])]
--         I.call printf [(intFmt, []), (fibVal, [])]
--         zero <- C.int32 0
--         I.ret zero
--     return ()

instance U.Codegen (Ann Ty.Type P.Literal) where
    codegen (Ann _ a@(P.IntLiteral i)) = C.int64 $ fromIntegral i
    codegen (Ann _ a@(P.FloatLiteral d)) = C.double $ realToFrac d
    codegen (Ann _ a@P.VoidLiteral) = error "codegen VoidLiteral"

instance U.CodegenTopLevel (Ann Ty.Type (P.Stmt Ty.Type)) where
    codegenTopLevel (Ann ty (P.DefnStmt defn)) = U.codegenTopLevel defn
    codegenTopLevel (Ann ty (P.ExprStmt expr)) = do
        let ir_type = U.irType ty
        blocks <- lift $ Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            entry <- Mn.block `Mn.named` "entry"
            ret <- U.codegen expr
            I.ret ret
        count <- lift $ gets $ \env -> env |> U.exprs |> length
        fn <- U.function (AST.mkName ("__anon_" ++ show count)) [] ir_type blocks
        lift $ U.pushExpr (ty, fn)
        return fn

instance U.CodegenTopLevel (Ann Ty.Type (P.Defn Ty.Type)) where
    codegenTopLevel (Ann ty (P.Op defn)) = U.codegenTopLevel defn
    codegenTopLevel (Ann ty (P.Fn defn)) = U.codegenTopLevel defn

instance U.CodegenTopLevel (Ann Ty.Type (P.OpDefn Ty.Type)) where
    codegenTopLevel (Ann fn_ty P.OpDefn {
        P.opdefn_op = op,
        P.opdefn_arity = arity,
        P.opdefn_args = args,
        P.opdefn_ret_ty = ret_ty,
        P.opdefn_body = body
    }) = do
        let ir_args = map (\(Ann ty arg) ->
             let irTy = U.irType ty
                 name = arg |> P.arg_name |> AST.mkName
             in (name, irTy)) args
        let ret_ty2 = ret_ty |> convertType
        let ir_type = ret_ty2 |> U.irType
        blocks <- lift $ Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            op_args <- forM args $ \(Ann ty arg) -> do
                v <- Mn.fresh `Mn.named` toShort (pack $ P.arg_name arg)
                return $ Op.LocalReference (U.irType ty) v
            entry <- Mn.block `Mn.named` "entry"
            let names = map (\(Ann _ arg) -> P.arg_name arg) args
            let types = map annotation args
            lift (op_args |> zip3 names types
                          |> map (\(name, ty, arg) -> U.pushVar name ty arg)
                          |> sequence)
            ret <- U.codegen body
            I.ret ret
        fn <- U.function (AST.mkName op) ir_args ir_type blocks
        lift $ U.pushDeclAndImpl op fn_ty (fn_ty, fn)
        return fn

instance U.CodegenTopLevel (Ann Ty.Type (P.FnDefn Ty.Type)) where
    codegenTopLevel (Ann fn_ty P.FnDefn {
        P.fndefn_name = name,
        P.fndefn_args = args,
        P.fndefn_ret_ty = ret_ty,
        P.fndefn_body = body
    }) = do
        let ir_args = map (\(Ann ty arg) ->
             (arg |> P.arg_name |> AST.mkName, U.irType ty))
             args
        let ret_ty2 = ret_ty |> convertType
        let ir_type = ret_ty2 |> U.irType
        blocks <- lift $ Mn.execIRBuilderT Mn.emptyIRBuilder $ do
            op_args <- forM args $ \(Ann ty arg) -> do
                v <- Mn.fresh `Mn.named` toShort (pack $ P.arg_name arg)
                return $ Op.LocalReference (U.irType ty) v
            entry <- Mn.block `Mn.named` "entry"
            let names = map (\(Ann _ arg) -> P.arg_name arg) args
            let types = map annotation args
            lift (op_args |> zip3 names types
                          |> map (\(name, ty, arg) -> U.pushVar name ty arg)
                          |> sequence)
            ret <- U.codegen body
            I.ret ret
        fn <- U.function (AST.mkName name) ir_args ir_type blocks
        lift $ U.pushDeclAndImpl name fn_ty (fn_ty, fn)
        return fn

instance U.Codegen (Ann Ty.Type (P.ForExpr Ty.Type)) where
    codegen (Ann ty P.ForExpr {
        P.for_init = Ann _ init,
        P.for_cond = Ann _ cond,
        P.for_oper = Ann _ oper,
        P.for_body = Ann _ body
    }) = error "Not implemented"

instance U.Codegen (Ann Ty.Type (P.IfExpr Ty.Type)) where
    codegen (Ann ty P.IfExpr {
        P.if_cond = Ann _ cond,
        P.if_then = Ann _ body,
        P.if_else = else_block
    }) = error "Not implemented"

instance U.Codegen (Ann Ty.Type (P.WhileExpr Ty.Type)) where
    codegen (Ann ty P.WhileExpr {
        P.while_cond = (Ann _ cond),
        P.while_body = (Ann _ body)
    }) = error "Not implemented"

instance U.Codegen (Ann Ty.Type (P.CallExpr Ty.Type)) where
    codegen (Ann ret_ty P.CallExpr {
        P.call_ident = name,
        P.call_args = args
    }) = do
        maybe_decl <- lift $ U.getDecl name
        let prototype = Ty.TFun Map.empty (map annotation args) ret_ty
        let impl = do
             fun_decl <- maybe_decl
             Map.lookup prototype $ U.impls fun_decl
        maybe
            (error $ "Function " ++ show name ++ " not found: " ++ show prototype)
            (\impl -> do
                gen_args <- args |> map U.codegen |> sequence
                gen_args |> map (, []) |> I.call impl)
            impl

instance U.Codegen (Ann Ty.Type (P.BinExpr Ty.Type)) where
    codegen (Ann ty P.BinExpr {
        P.bin_op = "=",
        P.bin_lhs = Ann _ lhs,
        P.bin_rhs = Ann _ rhs
    }) = error "Not implemented"
        -- annotated_rhs <- U.codegen rhs
        -- let ty = annotation annotated_rhs
        -- case lhs of
        --     P.Ident (Ann _ name) -> do
        --         lift $ pushVar name ty
        --         return $ Ann ty $ P.BinExpr {
        --             P.bin_op = "=",
        --             P.bin_lhs = Ann ty $ P.Ident $ Ann ty name,
        --             P.bin_rhs = annotated_rhs
        --         }
        --     _ -> throwE $ AssignErr AssignError
    codegen (Ann ret_ty P.BinExpr {
        P.bin_op = name,
        P.bin_lhs = Ann lhs_ty lhs,
        P.bin_rhs = Ann rhs_ty rhs
    }) = do
        maybe_decl <- lift $ U.getDecl name
        let impl = do
             fun_decl <- maybe_decl
             Map.lookup (Ty.TFun Map.empty [lhs_ty, rhs_ty] ret_ty) $ U.impls fun_decl
        maybe (error "Function not found") (\impl -> do
            gen_args <- [Ann lhs_ty lhs, Ann rhs_ty rhs] |> map U.codegen |> sequence
            gen_args |> map (, []) |> I.call impl)
            impl

instance U.Codegen (Ann Ty.Type (P.UnExpr Ty.Type)) where
    codegen (Ann ret_ty P.UnExpr {
        P.un_op = name,
        P.un_rhs = Ann rhs_ty rhs
    }) = do
        maybe_decl <- lift $ U.getDecl name
        let impl = do
             fun_decl <- maybe_decl
             Map.lookup (Ty.TFun Map.empty [rhs_ty] ret_ty) $ U.impls fun_decl
        maybe (error "Function not found") (\impl -> do
            gen_args <- [Ann rhs_ty rhs] |> map U.codegen |> sequence
            gen_args |> map (, []) |> I.call impl)
            impl

instance U.Codegen (Ann Ty.Type (P.Expr Ty.Type)) where
    codegen (Ann ty (P.For forExpr)) = U.codegen forExpr
    codegen (Ann ty (P.If ifExpr)) = U.codegen ifExpr
    codegen (Ann ty (P.While whileExpr)) = U.codegen whileExpr
    codegen (Ann ty (P.Ident (Ann _ ident))) = do
        found <- lift $ U.getVar ident
        maybe
            (error "Var not found")
            (return . snd)
            found
    codegen (Ann ty (P.Lit literal)) = U.codegen literal
    codegen (Ann ty (P.Call callExpr)) = U.codegen callExpr
    codegen (Ann ty (P.Un unExpr)) = U.codegen unExpr
    codegen (Ann ty (P.Bin binExpr)) = U.codegen binExpr

codegenAST :: P.AST Ty.Type -> AST.Module
codegenAST stmts =
    let (defns, exprs) = partition (\(Ann _ stmt) -> P.isDefnStmt stmt) stmts
        topLevel = do
            printf <- U.externVarArgs (AST.mkName "printf") [(AST.mkName "fmt", T.ptr T.i8)] T.i32
            doubleFmt <- U.stringPtr "%lf\n" "PRINT_DOUBLE"
            intFmt <- U.stringPtr "%ld\n" "PRINT_INT"
            voidFmt <- U.stringPtr "()\n" "PRINT_VOID"
            let printGenericProto = Ty.TFun (Map.fromList [(Ty.TV "a", [Ty.Trait "Show"])]) [Ty.TVar $ Ty.TV "a"] Ty.void
            lift $ U.pushDecl "print" printGenericProto
            printDouble <- M.function (AST.mkName "print_double") [(U.double, M.ParameterName "n")] U.void $ \[n] -> mdo
                entry <- Mn.block `Mn.named` "entry"
                I.call printf [(doubleFmt, []), (n, [])]
                return ()
            lift $ U.pushImpl "print" (Ty.TFun Map.empty [Ty.float] Ty.void) printDouble
            printInt <- M.function (AST.mkName "print_int") [(U.int, M.ParameterName "n")] U.void $ \[n] -> mdo
                entry <- Mn.block `Mn.named` "entry"
                I.call printf [(intFmt, []), (n, [])]
                return ()
            lift $ U.pushImpl "print" (Ty.TFun Map.empty [Ty.int] Ty.void) printInt
            prelude
            mapM_ U.codegenTopLevel (defns ++ reverse exprs)
            exps <- lift $ gets $ \env -> U.exprs env
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
    U.vars = [],
    U.decls = Map.empty,
    U.exprs = []
}
