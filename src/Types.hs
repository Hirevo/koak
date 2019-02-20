{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import Misc
import Data.String

import Data.List (intercalate, find)

import qualified Data.Map as Map

type Name = String

newtype TCon = TC String
    deriving (Eq, Ord)
instance Show TCon where
    show (TC ty) = ty
instance IsString TCon where
    fromString = TC

int, double, bool, void :: Type
int = TCon "int"
double = TCon "double"
bool = TCon "bool"
void = TCon "void"

newtype TVar =
    TV Name
    deriving (Eq, Ord)
instance Show TVar where
    show (TV var) = var
instance IsString TVar where
    fromString = TV

data Type =
    TCon TCon
    | TVar TVar
    | TFun (Map.Map TVar [Trait]) [Type] Type
    deriving (Eq, Ord)
instance Show Type where
    show = \case
        TCon ty -> show ty
        TVar var -> show var
        TFun constraints args ret ->
            let vars = do
                 (var, traits) <- Map.toList constraints
                 if null traits
                     then return $ show var
                     else return $ show var <> ": " <> (traits |> map show |> intercalate " + ")
                vars_decl = if null vars then "" else "<" <> (vars |> intercalate ", ") <> ">"
            in vars_decl <> "(" <> (args |> map show |> intercalate ", ") <> ") -> "
                <> show ret
instance IsString Type where
    fromString = TVar . TV
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
instance IsString Trait where
    fromString = Trait

traitsTable :: Map.Map Trait [TCon]
traitsTable = Map.fromList [ ("Num",        ["int", "double"]),
                             ("Integral",   ["int"]),
                             ("Fractional", ["double"]),
                             ("Eq",         ["int", "double", "bool"]),
                             ("Ord",        ["int", "double"]),
                             ("Default",    ["int", "double", "bool"]) ]

builtinBinaryOps :: Map.Map Name Type
builtinBinaryOps = Map.fromList [
        ( "+", TFun (Map.fromList [("T", ["Num"])]) ["T", "T"] "T"),
        ( "-", TFun (Map.fromList [("T", ["Num"])]) ["T", "T"] "T"),
        ( "*", TFun (Map.fromList [("T", ["Num"])]) ["T", "T"] "T"),
        ( "/", TFun (Map.fromList [("T", ["Num"])]) ["T", "T"] "T"),
        ( "<", TFun (Map.fromList [("T", ["Ord"])]) ["T", "T"] int),
        ( ">", TFun (Map.fromList [("T", ["Ord"])]) ["T", "T"] int),
        ("==", TFun (Map.fromList [("T", ["Eq"])]) ["T", "T"] int),
        ("!=", TFun (Map.fromList [("T", ["Eq"])]) ["T", "T"] int),
        ( ":", TFun (Map.fromList [("T", []), ("U", [])]) ["T", "U"] "U")
    ]

builtinUnaryOps :: Map.Map Name Type
builtinUnaryOps = Map.fromList [
        ("!", TFun (Map.fromList [("T", ["Num"])]) ["T"] int),
        ("-", TFun (Map.fromList [("T", ["Num"])]) ["T"] "T")
    ]

builtinFunctions :: Map.Map Name Type
builtinFunctions = Map.fromList [
        ("default", TFun (Map.fromList [("T", ["Default"])]) [] "T")
    ]

isFun :: Type -> Bool
isFun TFun{} = True
isFun _ = False

isGeneric :: Type -> Bool
isGeneric (TFun constraints _ _) | Map.size constraints > 0 = True
isGeneric _ = False
