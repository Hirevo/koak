{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T

import Data.Char (ord)

stringPtr :: M.MonadModuleBuilder m => String -> AST.Name -> m AST.Operand
stringPtr str nm = do
    let asciiVals = map (fromIntegral . ord) str
        llvmVals  = map (Cst.Int 8) (asciiVals ++ [0])
        char      = T.IntegerType 8
        charStar  = T.ptr char
        charArray = Cst.Array char llvmVals
        ty        = Tpd.typeOf charArray
    M.emitDefn $ AST.GlobalDefinition Glb.globalVariableDefaults {
          Glb.name        = nm
        , Glb.type'       = ty
        , Glb.linkage     = Lnk.External
        , Glb.isConstant  = True
        , Glb.initializer = Just charArray
        , Glb.unnamedAddr = Just AST.GlobalAddr
    }
    pure $ AST.ConstantOperand $ Cst.BitCast (Cst.GlobalReference (T.ptr ty) nm) charStar

fib :: AST.Module
fib = M.buildModule "bootstrap" $ mdo
    arr <- stringPtr "%d\n" "PRINT_STR"
    printf <- M.extern (AST.mkName "printf") [T.ptr T.i8, T.i32] T.i32
    fib <- M.function (AST.mkName "fibonnacci") [(T.i32, M.ParameterName "n")] T.i32 $ \[n] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        I.ret n
    M.function (AST.mkName "main") [(T.i32, M.ParameterName "ac")] T.i32 $ \[ac] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        fibVal <- I.call fib [(ac, [])]
        I.call printf [(arr, []), (fibVal, [])]
        zero <- C.int32 0
        I.ret zero
