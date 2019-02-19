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
    | MultipleDefnError Name [Scheme]
    | AssignError
    | NotImplTraitError Type Trait
    | TraitNotInScopeError Trait
    | CantSpecializeError Name Type Type
    | CantConstructInfiniteType TVar Type
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
                <> show trait <> "' trait"
        TraitNotInScopeError trait ->
            "TraitNotInScopeError: The trait '" <> show trait <> "' is not defined"
        CantSpecializeError name gen con ->
            "CantSpecializeError: (could not specialize '" <> name <> "' of type " <> show gen
                <> " for type " <> show con
instance Semigroup Error where
    err <> _ = err
instance Monoid Error where
    mappend = (<>)
    mempty = AssignError

-- Usable to find close matches for NotInScope errors.
levenshtein :: String -> String -> Int
levenshtein a b | min (length a) (length b) == 0 = max (length a) (length b)
levenshtein (a:as) (b:bs) = minimum [ levenshtein as (b:bs) + 1,
                                      levenshtein (a:as) bs + 1,
                                      levenshtein as bs + if a == b then 0 else 1 ]
