{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen.Utils where

import Misc
import Annotation
import Control.Monad.State.Lazy
import Control.Applicative

import Data.Char (ord)

import qualified Types as Ty
import qualified Parser.Lang as P
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Cst
import qualified LLVM.AST.FunctionAttribute as FnAttr
import qualified LLVM.AST.Global as Glb
import qualified LLVM.AST.Linkage as Lnk
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as Tpd
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as Mn
import qualified Data.Map as Map

data FnDecl = FnDecl {
    ty :: Ty.Type,
    body :: Maybe (P.Stmt Ty.Type),
    impls :: Map.Map Ty.Type AST.Operand
} deriving (Show, Eq)
data Env = Env {
    vars :: [Map.Map Ty.Name (Ty.Type, AST.Operand)],
    decls :: Map.Map Ty.Name FnDecl,
    exprs :: [(Ty.Type, AST.Operand)]
} deriving (Show, Eq)

newScope, dropScope :: State Env ()
newScope = modify $ \env -> env { vars = Map.empty : vars env }
dropScope = modify $ \env -> env { vars = tail $ vars env }

pushDecl :: Ty.Name -> Ty.Type -> Maybe (P.Stmt Ty.Type) -> State Env ()
pushDecl name ty body = modify $ \env -> env { decls = Map.insert name FnDecl{ ty, body, impls = Map.empty } $ decls env }
pushImpl :: Ty.Name -> Ty.Type -> AST.Operand -> State Env ()
pushImpl name ty op = modify $ \env -> env { decls = Map.adjust (\elem -> elem { impls = Map.insert ty op $ impls elem }) name $ decls env }
pushDeclAndImpl :: Ty.Name -> Ty.Type -> (Ty.Type, AST.Operand) -> State Env ()
pushDeclAndImpl name ty (impl_ty, op) = modify $ \env -> env { decls = Map.insert name FnDecl{ ty, body = Nothing, impls = Map.singleton impl_ty op } $ decls env }
pushVar :: Ty.Name -> Ty.Type -> AST.Operand -> State Env ()
pushVar name ty operand = modify $ \env ->
    if null $ vars env
        then env { vars = [Map.singleton name (ty, operand)] }
        else env { vars = Map.insert name (ty, operand) (head $ vars env) : tail (vars env) }
pushExpr :: (Ty.Type, AST.Operand) -> State Env ()
pushExpr expr = modify $ \env -> env { exprs = expr : exprs env }

getVar :: Ty.Name -> State Env (Maybe (Ty.Type, AST.Operand))
getVar name = gets $ \env -> env |> vars |> map (Map.lookup name) |> foldl (<|>) Nothing
getDecl :: Ty.Name -> State Env (Maybe FnDecl)
getDecl name = gets $ \env -> env |> decls |> Map.lookup name
getImpl :: Ty.Name -> Ty.Type -> State Env (Maybe AST.Operand)
getImpl name ty = do
    maybe_defn <- gets $ \env -> env |> decls |> Map.lookup name
    return $ do
        defn <- maybe_defn
        Map.lookup ty $ impls defn

class Codegen a where
    codegen :: a -> Mn.IRBuilderT (M.ModuleBuilderT (State Env)) AST.Operand

class CodegenTopLevel a where
    codegenTopLevel :: a -> M.ModuleBuilderT (State Env) AST.Operand

int, double, void :: T.Type
int = T.i64
double = T.double
void = T.void

irType :: Ty.Type -> T.Type
irType (Ty.TCon (Ty.TC "int")) = int
irType (Ty.TCon (Ty.TC "double")) = double
irType (Ty.TCon (Ty.TC "void")) = Codegen.Utils.void
irType (Ty.TFun _ args ret_ty) = T.FunctionType (irType ret_ty) (args |> map irType) False
irType ty = error $ show ty

-- | An constant static string pointer
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

function :: M.MonadModuleBuilder m
    => AST.Name -> [(AST.Name, T.Type)] -> T.Type -> [AST.BasicBlock] -> m AST.Operand
function name argtys retty body = do
    M.emitDefn $ AST.GlobalDefinition $ Glb.functionDefaults {
        Glb.name = name,
        Glb.parameters = ([AST.Parameter ty name [] | (name, ty) <- argtys], False),
        Glb.returnType = retty,
        Glb.basicBlocks = body
    }
    let funty = T.ptr $ AST.FunctionType retty (map snd argtys) False
    pure $ AST.ConstantOperand $ Cst.GlobalReference funty name

inlineFunction :: M.MonadModuleBuilder m
  => AST.Name
  -> [(T.Type, M.ParameterName)]
  -> AST.Type
  -> ([AST.Operand] -> Mn.IRBuilderT m ())
  -> m AST.Operand
inlineFunction name argtys retty body = do
    let tys = fst <$> argtys
    (paramNames, blocks) <- Mn.runIRBuilderT Mn.emptyIRBuilder $ do
        paramNames <- forM argtys $ \(_, paramName) -> case paramName of
            M.NoParameterName -> Mn.fresh
            M.ParameterName p -> Mn.fresh `Mn.named` p
        body $ zipWith AST.LocalReference tys paramNames
        return paramNames
    let def = AST.GlobalDefinition Glb.functionDefaults {
        Glb.name = name,
        Glb.functionAttributes = [Right FnAttr.AlwaysInline],
        Glb.parameters = (zipWith (\ty nm -> AST.Parameter ty nm []) tys paramNames, False),
        Glb.returnType = retty,
        Glb.basicBlocks = blocks
    }
    let funty = T.ptr $ AST.FunctionType retty (fst <$> argtys) False
    M.emitDefn def
    pure $ AST.ConstantOperand $ Cst.GlobalReference funty name

-- | An external function definition
extern :: M.MonadModuleBuilder m
    => AST.Name -> [(AST.Name, T.Type)] -> T.Type -> m AST.Operand
extern nm argtys retty = do
    M.emitDefn $ AST.GlobalDefinition Glb.functionDefaults {
        Glb.name        = nm,
        Glb.linkage     = Lnk.External,
        Glb.parameters  = ([AST.Parameter ty name [] | (name, ty) <- argtys], False),
        Glb.returnType  = retty
    }
    let funty = T.ptr $ AST.FunctionType retty (map snd argtys) False
    pure $ AST.ConstantOperand $ Cst.GlobalReference funty nm

-- | An external variadic argument function definition
externVarArgs :: M.MonadModuleBuilder m
    => AST.Name -> [(AST.Name, T.Type)] -> T.Type -> m AST.Operand
externVarArgs nm argtys retty = do
    M.emitDefn $ AST.GlobalDefinition Glb.functionDefaults {
        Glb.name        = nm,
        Glb.linkage     = Lnk.External,
        Glb.parameters  = ([AST.Parameter ty name [] | (name, ty) <- argtys], True),
        Glb.returnType  = retty
    }
    let funty = T.ptr $ AST.FunctionType retty (map snd argtys) True
    pure $ AST.ConstantOperand $ Cst.GlobalReference funty nm

mangleType :: Ty.Type -> String
mangleType (Ty.TCon (Ty.TC ty)) = [head ty]

mangleFunction :: String -> [Ty.Type] -> Ty.Type -> String
mangleFunction name args ret_ty =
    name ++ "_" ++ concatMap mangleType args ++ "_" ++ mangleType ret_ty
