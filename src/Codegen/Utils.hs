module Codegen.Utils where

import Data.Char (ord)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Module as M

int, double, void :: T.Type
int = T.i64
double = T.double
void = T.void

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
