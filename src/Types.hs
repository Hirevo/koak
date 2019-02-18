module Types where

import Misc

import Data.List (intercalate, find)

import qualified Data.Map as Map

type Name = String

newtype TCon = TC String
    deriving (Eq, Ord)
instance Show TCon where
    show (TC ty) = ty

int, double, void :: Type
int = TCon $ TC "int"
double = TCon $ TC "double"
void = TCon $ TC "void"

newtype TVar =
    TV Name
    deriving (Eq, Ord)
instance Show TVar where
    show (TV var) = var

data Type =
    TCon TCon
    | TVar TVar
    | TFun (Map.Map TVar [Trait]) [Type] Type
    deriving (Eq, Ord)
instance Show Type where
    show (TCon ty) = show ty
    show (TVar var) = show var
    show (TFun constraints args ret) =
        let vars = do
             (var, traits) <- Map.toList constraints
             if null traits
                 then return $ show var
                 else return $ show var ++ ": " ++ (traits |> map show |> intercalate " + ")
            vars_decl = if null vars then "" else "<" ++ (vars |> intercalate ", ") ++ ">"
        in vars_decl ++ "(" ++ (args |> map show |> intercalate ", ") ++ ") -> "
            ++ show ret
isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False
getFuncReturnType :: Type -> Type
getFuncReturnType (TFun _ _ ty) = ty
getFuncReturnType _ = error "Asked for the return type of not a function"

newtype Trait =
    Trait Name
    deriving (Eq, Ord)
instance Show Trait where
    show (Trait name) = name

traitsTable :: Map.Map Trait [TCon]
traitsTable = Map.fromList [ (Trait "Num", [TC "int", TC "double"]),
                             (Trait "Integral", [TC "int"]),
                             (Trait "Fractional", [TC "double"]),
                             (Trait "Eq", [TC "int", TC "double"]),
                             (Trait "Ord", [TC "int", TC "double"]) ]

builtinBinaryOps :: Map.Map Name Type
builtinBinaryOps = Map.fromList [
        ( "+", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "-", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "*", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "/", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T", TVar $ TV "T"] (TVar $ TV "T")),
        ( "<", TFun (Map.fromList [(TV "T", [Trait "Ord"])]) [TVar $ TV "T", TVar $ TV "T"] int),
        ( ">", TFun (Map.fromList [(TV "T", [Trait "Ord"])]) [TVar $ TV "T", TVar $ TV "T"] int),
        ("==", TFun (Map.fromList [(TV "T", [Trait "Eq"])]) [TVar $ TV "T", TVar $ TV "T"] int),
        ("!=", TFun (Map.fromList [(TV "T", [Trait "Eq"])]) [TVar $ TV "T", TVar $ TV "T"] int),
        ( ":", TFun (Map.fromList [(TV "T", []), (TV "U", [])]) [TVar $ TV "T", TVar $ TV "U"] (TVar $ TV "U"))
    ]

builtinUnaryOps :: Map.Map Name Type
builtinUnaryOps = Map.fromList [
        ("!", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T"] int),
        ("-", TFun (Map.fromList [(TV "T", [Trait "Num"])]) [TVar $ TV "T"] (TVar $ TV "T"))
    ]

builtinFunctions :: Map.Map Name Type
builtinFunctions = Map.fromList [
        ("default", TFun (Map.fromList [(TV "T", [Trait "Default"])]) [] (TVar $ TV "T"))
    ]

isFun :: Type -> Bool
isFun TFun{} = True
isFun _ = False

isGeneric :: Type -> Bool
isGeneric (TFun constraints _ _) | Map.size constraints > 0 = True
isGeneric _ = False
