{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Prelude where

import Data.Char (ord)

import qualified Types as Ty
import qualified Codegen.Utils as U
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

addInt, addDouble :: M.MonadModuleBuilder m => m AST.Operand
addInt =
    let name = AST.mkName "add_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.add a b
        I.ret result
addDouble =
    let name = AST.mkName "add_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fadd a b
        I.ret result

subInt, subDouble :: M.MonadModuleBuilder m => m AST.Operand
subInt =
    let name = AST.mkName "sub_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.sub a b
        I.ret result
subDouble =
    let name = AST.mkName "sub_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fsub a b
        I.ret result

multInt, multDouble :: M.MonadModuleBuilder m => m AST.Operand
multInt =
    let name = AST.mkName "mult_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.mul a b
        I.ret result
multDouble =
    let name = AST.mkName "mult_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fmul a b
        I.ret result

divInt, divDouble :: M.MonadModuleBuilder m => m AST.Operand
divInt =
    let name = AST.mkName "div_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.sdiv a b
        I.ret result
divDouble =
    let name = AST.mkName "div_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.fdiv a b
        I.ret result

ltInt, ltDouble :: M.MonadModuleBuilder m => m AST.Operand
ltInt =
    let name = AST.mkName "lt_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.SLT a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
ltDouble =
    let name = AST.mkName "lt_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OLT a b
        true <- C.double 1
        false <- C.double 0
        result <- I.select cond true false
        I.ret result

gtInt, gtDouble :: M.MonadModuleBuilder m => m AST.Operand
gtInt =
    let name = AST.mkName "gt_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.SGT a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
gtDouble =
    let name = AST.mkName "gt_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OGT a b
        true <- C.double 1
        false <- C.double 0
        result <- I.select cond true false
        I.ret result

eqInt, eqDouble :: M.MonadModuleBuilder m => m AST.Operand
eqInt =
    let name = AST.mkName "eq_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.EQ a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
eqDouble =
    let name = AST.mkName "eq_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OEQ a b
        true <- C.double 1
        false <- C.double 0
        result <- I.select cond true false
        I.ret result

neqInt, neqDouble :: M.MonadModuleBuilder m => m AST.Operand
neqInt =
    let name = AST.mkName "neq_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.NE a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
neqDouble =
    let name = AST.mkName "neq_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.ONE a b
        true <- C.double 1
        false <- C.double 0
        result <- I.select cond true false
        I.ret result

negInt, negDouble :: M.MonadModuleBuilder m => m AST.Operand
negInt =
    let name = AST.mkName "neg_int"
        params = [(U.int, M.ParameterName "a")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        neg <- C.int64 (-1)
        result <- I.mul a neg
        I.ret result
negDouble =
    let name = AST.mkName "neg_double"
        params = [(U.double, M.ParameterName "a")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        neg <- C.double (-1)
        result <- I.fmul a neg
        I.ret result

invInt, invDouble :: M.MonadModuleBuilder m => m AST.Operand
invInt =
    let name = AST.mkName "inv_int"
        params = [(U.int, M.ParameterName "a")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        one <- C.int64 1
        zero <- C.int64 0
        cond <- I.icmp IPred.EQ a zero
        result <- I.select cond one zero
        I.ret result
invDouble =
    let name = AST.mkName "inv_double"
        params = [(U.double, M.ParameterName "a")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        one <- C.double 1
        zero <- C.double 0
        cond <- I.fcmp FPred.OEQ a zero
        result <- I.select cond one zero
        I.ret result