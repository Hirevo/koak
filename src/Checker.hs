module Checker where

import Unifier

data Type =
    TVar String
    | TArr Type Type
    deriving (Show, Eq)

instance Unify Type where
    unify t1 t2 | t1 == t2 = Just t1
    unify _ _ = Nothing

apply :: Type -> Type -> Maybe Type
apply (TArr t1 t2) t3 | t1 == t3 = Just t2
apply _ _ = Nothing
