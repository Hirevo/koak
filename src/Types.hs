module Types where

import Data.List (intersperse, find)
import qualified Data.Map as Map

type Name = String

newtype TCon = TC String
    deriving (Eq, Ord)
instance Show TCon where
    show (TC ty) = ty

int, float, void :: Type
int = TCon $ TC "int"
float = TCon $ TC "double"
void = TCon $ TC "void"

newtype TVar =
    TV Name
    deriving (Eq, Ord)
instance Show TVar where
    show (TV var) = var

type Constraints = Map.Map TVar [Trait]

data Type =
    TCon TCon
    | TVar TVar
    | TFun Constraints [Type] Type
    deriving (Eq, Ord)
instance Show Type where
    show (TCon ty) = show ty
    show (TVar var) = show var
    show (TFun constraints args ret) =
        let vars = do
             (var, traits) <- Map.toList constraints
             if null traits
                 then return $ show var
                 else return $ show var ++ ": " ++ concat (intersperse " + " $ map show traits)
            vars_decl = if null vars then "" else "<" ++ concat (intersperse ", " vars) ++ ">"
        in vars_decl ++ "(" ++ concat (intersperse ", " $ map show args) ++ ") -> "
            ++ show ret

newtype Trait =
    Trait Name
    deriving (Eq, Ord)
instance Show Trait where
    show (Trait name) = name

traitsTable :: Map.Map Trait [TCon]
traitsTable = Map.fromList [ (Trait "Num", [TC "int", TC "double"])
                           , (Trait "Eq",  [TC "int", TC "double", TC "void"])
                           , (Trait "Ord", [TC "int", TC "double"]) ]

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

isFun :: Type -> Bool
isFun TFun{} = True
isFun _ = False

isGeneric :: Type -> Bool
isGeneric (TFun constraints _ _) | Map.size constraints > 0 = True
isGeneric _ = False
