{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Codegen.Prelude where

import Annotation
import Control.Monad.State.Lazy

import Data.Char (ord)

import qualified Types as Ty
import qualified Codegen.Utils as U
import qualified Parser.Lib as L
import qualified Parser.Lang as P
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Instruction as Ist
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

prelude :: U.CodegenTopLevel ()
prelude = do
    addI <- addInt
    addD <- addDouble
    subI <- subInt
    subD <- subDouble
    multI <- multInt
    multD <- multDouble
    divI <- divInt
    divD <- divDouble
    modI <- modInt
    modD <- modDouble
    ltI <- ltInt
    ltD <- ltDouble
    gtI <- gtInt
    gtD <- gtDouble
    eqI <- eqInt
    eqD <- eqDouble
    eqB <- eqBool
    neqI <- neqInt
    neqD <- neqDouble
    neqB <- neqBool
    negI <- negInt
    negD <- negDouble
    invI <- invInt
    invD <- invDouble
    defI <- defaultInt
    defD <- defaultDouble
    let ty1 = [("T", ["Num"])] Ty.:=> ["T", "T"] Ty.:-> "T"
    let ty2 = [("T", ["Ord"])] Ty.:=> ["T", "T"] Ty.:-> Ty.int
    let ty3 = [("T", ["Eq"])] Ty.:=> ["T", "T"] Ty.:-> Ty.int
    let ty4 = [("T", []), ("U", [])] Ty.:=> ["T", "U"] Ty.:-> "U"
    let ty5 = [("T", ["Num"])] Ty.:=> ["T"] Ty.:-> "T"
    let ty6 = [("T", ["Eq"])] Ty.:=> ["T"] Ty.:-> Ty.int
    let ty7 = [("T", ["Default"])] Ty.:=> [] Ty.:-> "T"
    U.pushDecl "binary_+" ty1 Nothing
    U.pushDecl "binary_-" ty1 Nothing
    U.pushDecl "binary_*" ty1 Nothing
    U.pushDecl "binary_/" ty1 Nothing
    U.pushDecl "binary_%" ty1 Nothing
    U.pushDecl "binary_<" ty2 Nothing
    U.pushDecl "binary_>" ty2 Nothing
    U.pushDecl "binary_==" ty3 Nothing
    U.pushDecl "binary_!=" ty3 Nothing
    U.pushDecl "binary_:" ty4 (Just (P.Defn (P.Binary (P.LeftAssoc 25)) "binary_:"
        [Ann (L.nullRange, "T") (P.Arg "a" "T"), Ann (L.nullRange, "U") (P.Arg "b" "U")] "U"
        (Ann (L.nullRange, "U") (P.Ident "b"))))
    U.pushDecl "unary_-" ty5 Nothing
    U.pushDecl "unary_!" ty6 Nothing
    U.pushDecl "default" ty7 Nothing
    U.pushImpl "binary_+" ([Ty.int, Ty.int] Ty.:-> Ty.int) addI
    U.pushImpl "binary_+" ([Ty.double, Ty.double] Ty.:-> Ty.double) addD
    U.pushImpl "binary_-" ([Ty.int, Ty.int] Ty.:-> Ty.int) subI
    U.pushImpl "binary_-" ([Ty.double, Ty.double] Ty.:-> Ty.double) subD
    U.pushImpl "binary_*" ([Ty.int, Ty.int] Ty.:-> Ty.int) multI
    U.pushImpl "binary_*" ([Ty.double, Ty.double] Ty.:-> Ty.double) multD
    U.pushImpl "binary_/" ([Ty.int, Ty.int] Ty.:-> Ty.int) divI
    U.pushImpl "binary_/" ([Ty.double, Ty.double] Ty.:-> Ty.double) divD
    U.pushImpl "binary_%" ([Ty.int, Ty.int] Ty.:-> Ty.int) modI
    U.pushImpl "binary_%" ([Ty.double, Ty.double] Ty.:-> Ty.double) modD
    U.pushImpl "binary_<" ([Ty.int, Ty.int] Ty.:-> Ty.int) ltI
    U.pushImpl "binary_<" ([Ty.double, Ty.double] Ty.:-> Ty.int) ltD
    U.pushImpl "binary_>" ([Ty.int, Ty.int] Ty.:-> Ty.int) gtI
    U.pushImpl "binary_>" ([Ty.double, Ty.double] Ty.:-> Ty.int) gtD
    U.pushImpl "binary_==" ([Ty.int, Ty.int] Ty.:-> Ty.int) eqI
    U.pushImpl "binary_==" ([Ty.double, Ty.double] Ty.:-> Ty.int) eqD
    U.pushImpl "binary_==" ([Ty.bool, Ty.bool] Ty.:-> Ty.int) eqB
    U.pushImpl "binary_!=" ([Ty.int, Ty.int] Ty.:-> Ty.int) neqI
    U.pushImpl "binary_!=" ([Ty.double, Ty.double] Ty.:-> Ty.int) neqD
    U.pushImpl "binary_!=" ([Ty.bool, Ty.bool] Ty.:-> Ty.int) neqB
    U.pushImpl "unary_-" ([Ty.int] Ty.:-> Ty.int) negI
    U.pushImpl "unary_-" ([Ty.double] Ty.:-> Ty.double) negD
    U.pushImpl "unary_!" ([Ty.int] Ty.:-> Ty.int) invI
    U.pushImpl "unary_!" ([Ty.double] Ty.:-> Ty.int) invD
    U.pushImpl "default" ([] Ty.:-> Ty.int) defI
    U.pushImpl "default" ([] Ty.:-> Ty.double) defD

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

modInt, modDouble :: M.MonadModuleBuilder m => m AST.Operand
modInt =
    let name = AST.mkName "mod_int"
        params = [(U.int, M.ParameterName "a"), (U.int, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- Mn.emitInstr (Tpd.typeOf a) $ Ist.SRem a b []
        I.ret result
modDouble =
    let name = AST.mkName "mod_double"
        params = [(U.double, M.ParameterName "a"), (U.double, M.ParameterName "b")]
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        result <- I.frem a b
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
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OLT a b
        true <- C.int64 1
        false <- C.int64 0
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
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OGT a b
        true <- C.int64 1
        false <- C.int64 0
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
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.OEQ a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
eqBool =
    let name = AST.mkName "eq_bool"
        params = [(U.bool, M.ParameterName "a"), (U.bool, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.EQ a b
        true <- C.int64 1
        false <- C.int64 0
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
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.fcmp FPred.ONE a b
        true <- C.int64 1
        false <- C.int64 0
        result <- I.select cond true false
        I.ret result
neqBool =
    let name = AST.mkName "neq_bool"
        params = [(U.bool, M.ParameterName "a"), (U.bool, M.ParameterName "b")]
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a, b] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        cond <- I.icmp IPred.NE a b
        true <- C.int64 1
        false <- C.int64 0
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
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[a] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        one <- C.int64 1
        zero <- C.int64 0
        fzero <- C.double 0
        cond <- I.fcmp FPred.OEQ a fzero
        result <- I.select cond one zero
        I.ret result

defaultInt, defaultDouble :: M.MonadModuleBuilder m => m AST.Operand
defaultInt =
    let name = AST.mkName "default_int"
        params = []
        ret_ty = U.int
    in U.inlineFunction name params ret_ty $ \[] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        zero <- C.int64 0
        I.ret zero
defaultDouble =
    let name = AST.mkName "default_double"
        params = []
        ret_ty = U.double
    in U.inlineFunction name params ret_ty $ \[] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        zero <- C.double 0
        I.ret zero
