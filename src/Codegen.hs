{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import LLVM.IRBuilder.Module as M
import LLVM.IRBuilder.Monad as Mn
import LLVM.IRBuilder.Constant as C
import LLVM.AST.Constant as Cst
import LLVM.AST.Global as Glb
import LLVM.AST.Typed as Tpd
import LLVM.AST.Linkage as Lnk
import LLVM.IRBuilder.Instruction as I
import LLVM.AST as AST
import LLVM.AST.Type as T

import Data.Char (ord)

stringPtr :: MonadModuleBuilder m => String -> Name -> m Operand
stringPtr str nm = do
    let asciiVals = map (fromIntegral . ord) str
        llvmVals  = map (Cst.Int 8) (asciiVals ++ [0])
        char      = IntegerType 8
        charStar  = ptr char
        charArray = Cst.Array char llvmVals
        ty        = Tpd.typeOf charArray
    emitDefn $ GlobalDefinition globalVariableDefaults {
          name        = nm
        , Glb.type'   = ty
        , linkage     = Lnk.External
        , isConstant  = True
        , initializer = Just charArray
        , unnamedAddr = Just GlobalAddr
    }
    pure $ ConstantOperand $ Cst.BitCast (Cst.GlobalReference (ptr ty) nm) charStar

fib :: Integer -> AST.Module
fib i = M.buildModule "bootstrap" $ mdo
    arr <- stringPtr "%d\n" "PRINT_STR"
    printf <- M.extern (Name "printf") [T.ptr T.i8, T.i32] T.i32
    fib <- M.function (Name "fibonnacci") [(T.i32, M.ParameterName "n")] T.i32 $ \[n] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        I.ret n
    M.function (Name "main") [] T.i32 $ \[] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        input <- C.int32 i
        fibVal <- I.call fib [(input, [])]
        I.call printf [(arr, []), (fibVal, [])]
        zero <- C.int32 0
        ret zero
