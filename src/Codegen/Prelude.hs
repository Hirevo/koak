{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Prelude where

import Data.Char (ord)

import qualified Codegen.Utils as U
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn

prelude :: M.MonadModuleBuilder m => [m AST.Operand]
prelude = [
        addInt, addDouble,
        subInt, subDouble,
        multInt, multDouble,
        divInt, divDouble
    ]

addInt, addDouble :: M.MonadModuleBuilder m => m AST.Operand
addInt =
    let name = AST.mkName "addInt"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.add a b
        I.ret result
addDouble =
    let name = AST.mkName "addDouble"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fadd a b
        I.ret result

subInt, subDouble :: M.MonadModuleBuilder m => m AST.Operand
subInt =
    let name = AST.mkName "subInt"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.sub a b
        I.ret result
subDouble =
    let name = AST.mkName "subDouble"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fsub a b
        I.ret result

multInt, multDouble :: M.MonadModuleBuilder m => m AST.Operand
multInt =
    let name = AST.mkName "multInt"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.mul a b
        I.ret result
multDouble =
    let name = AST.mkName "multDouble"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fmul a b
        I.ret result

divInt, divDouble :: M.MonadModuleBuilder m => m AST.Operand
divInt =
    let name = AST.mkName "divInt"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.sdiv a b
        I.ret result
divDouble =
    let name = AST.mkName "divDouble"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in M.function name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fdiv a b
        I.ret result
