{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Errors where

import Misc
import Types

import Data.List (intercalate)

data Error =
    TypeError Type Type
    | ArgCountError Int Int
    | NotInScopeError Name
    | MultipleDefnError Name [Type]
    | AssignError
    | NotImplTraitError Type Trait
    | TraitNotInScopeError Trait
    deriving (Eq)
instance Show Error where
    show = \case
        TypeError t1 t2 ->
            "TypeError (could not unify " <> show t1 <> " and " <> show t2 <> ")"
        ArgCountError expected got ->
            "ArgCountError (expected: " <> show expected <> ", got: " <> show got <> ")"
        NotInScopeError name ->
            "NotInScopeError: '" <> name <> "'"
        MultipleDefnError name defns ->
            "MultipleDefnError: '" <> name <> "' has multiple definitions:\n"
                <> (defns |> map ((<>) "\t- " . show) |> intercalate "\n")
        AssignError ->
            "AssignError (expected identifier on the left-hand side of an assignment)"
        NotImplTraitError ty trait ->
            "NotImplTraitError: The type '" <> show ty <> "' does not implement the '"
                <> show trait <> "' trait."
        TraitNotInScopeError trait ->
            "TraitNotInScopeError: The trait '" <> show trait <> "' is not defined."
