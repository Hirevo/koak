module Unifier where

class Unify a where
    unify :: a -> a -> Maybe a
    unify = (<->)
    (<->) :: a -> a -> Maybe a
    (<->) = unify
