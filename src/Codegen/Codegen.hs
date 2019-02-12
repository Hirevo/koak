{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Codegen where
    
import Data.Char (ord)

import qualified Codegen.Prelude as Pre
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

fib :: AST.Module
fib = M.buildModule "bootstrap" $ mdo
    arr <- U.stringPtr "%d\n" "PRINT_STR"
    printf <- M.extern (AST.mkName "printf") [T.ptr T.i8, T.i32] T.i32
    sequence Pre.prelude
    fib <- M.function (AST.mkName "fibonnacci") [(T.i32, M.ParameterName "n")] T.i32 $ \[n] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        I.ret n
    M.function (AST.mkName "main") [(T.i32, M.ParameterName "ac")] T.i32 $ \[ac] -> mdo
        entry <- Mn.block `Mn.named` "entry"
        fibVal <- I.call fib [(ac, [])]
        I.call printf [(arr, []), (fibVal, [])]
        zero <- C.int32 0
        I.ret zero
