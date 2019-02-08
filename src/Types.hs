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
        in "<" ++ concat (intersperse ", " vars) ++ ">"
            ++ "(" ++ concat (intersperse ", " $ map show args) ++ ") -> "
            ++ show ret

newtype Trait =
    Trait Name
    deriving (Eq, Ord)
instance Show Trait where
    show (Trait name) = name

data Scheme =
    Forall Constraints Type
    deriving (Eq, Ord)
instance Show Scheme where
    show (Forall constraints ty) =
        let vars = do
             (var, traits) <- Map.toList constraints
             if null traits
                 then return $ show var
                 else return $ show var ++ ": " ++ concat (intersperse " + " $ map show traits)
        in concat (intersperse ", " vars) ++ show ty

isFun :: Type -> Bool
isFun TFun{} = True
isFun _ = False
