{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import Misc
import Types

import Data.List (intercalate)

data Error =
    TypeErr TypeError
    | ArgCountErr ArgCountError
    | NotInScopeErr NotInScopeError
    | MultipleDefnErr MultipleDefnError
    | AssignErr AssignError
    | NotImplTraitErr NotImplTraitError
    | TraitNotInScopeErr TraitNotInScopeError
    deriving (Eq)
instance Show Error where
    show (TypeErr err) = show err
    show (ArgCountErr err) = show err
    show (NotInScopeErr err) = show err
    show (MultipleDefnErr err) = show err
    show (AssignErr err) = show err
    show (NotImplTraitErr err) = show err
    show (TraitNotInScopeErr err) = show err
data TypeError = TypeError {
    expected :: Type,
    got :: Type
} deriving (Eq)
instance Show TypeError where
    show TypeError { expected, got } =
        "TypeError (expected: " ++ show expected
            ++ ", got: " ++ show got ++ ")"
data ArgCountError = ArgCountError {
    expected :: Int,
    got :: Int
} deriving (Eq)
instance Show ArgCountError where
    show ArgCountError { expected, got } =
        "ArgCountError (expected: " ++ show expected
            ++ ", got: " ++ show got ++ ")"
newtype NotInScopeError = NotInScopeError {
    ident :: Name
} deriving (Eq)
instance Show NotInScopeError where
    show NotInScopeError { ident } = "NotInScopeError: '" ++ ident ++ "'"
data MultipleDefnError = MultipleDefnError {
    name :: Name,
    definitions :: [Type]
} deriving (Eq)
instance Show MultipleDefnError where
    show MultipleDefnError { name, definitions } = "MultipleDefnError: '" ++ name ++ "' has multiple definitions:\n"
        ++ (definitions |> map ((++) "\t- " . show) |> intercalate "\n")
data AssignError = AssignError deriving (Eq)
instance Show AssignError where
    show AssignError =
        "AssignError (expected identifier on the left-hand side of an assignment)"
data NotImplTraitError = NotImplTraitError {
    ty :: TCon,
    trait :: Trait
} deriving (Eq)
instance Show NotImplTraitError where
    show NotImplTraitError { ty, trait } =
        "NotImplTraitError: The type '" ++ show ty ++ "' does not implement the '"
            ++ show trait ++ "' trait."
newtype TraitNotInScopeError = TraitNotInScopeError {
    trait :: Trait
} deriving (Eq)
instance Show TraitNotInScopeError where
    show TraitNotInScopeError { trait } =
        "TraitNotInScopeError: The trait '" ++ show trait ++ "' is not defined."
