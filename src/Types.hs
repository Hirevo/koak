{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import Misc
import Data.String
import Data.Monoid

import Data.List (intercalate, find)

import qualified Data.Map as Map
import qualified Data.Set as Set

type Name = String

newtype TCon = TC String
    deriving (Eq, Ord)
instance Show TCon where
    show (TC ty) = ty
instance IsString TCon where
    fromString = TC

int, double, bool, void :: Type
int = TCon "integer"
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
    | [Type] :-> Type
    deriving (Eq, Ord)
infixl 9 :->
instance Show Type where
    show = \case
        TCon ty -> show ty
        TVar var -> show var
        args :-> ret ->
            let args_s = args |> map show |> intercalate ", "
            in "(" <> args_s <> ") -> " <> show ret
instance Substitutable Type where
    applySubst s@(Subst s') = \case
        TVar a -> Map.findWithDefault (TVar a) (TVar a) s'
        TCon tc -> TCon tc
        f :-> x    -> applySubst s f :-> applySubst s x
    freeTypeVars = \case
        TVar a -> Set.singleton a
        TCon _ -> Set.empty
        args :-> ret ->
            let ftv_args = args |> map freeTypeVars
                                |> foldl (<>) Set.empty
            in ftv_args <> freeTypeVars ret
instance IsString Type where
    fromString = TVar . TV
isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False
getFuncReturnType :: Type -> Type
getFuncReturnType (_ :-> ty) = ty
getFuncReturnType _ = error "Asked for the return type of not a function"

newtype Trait =
    Trait Name
    deriving (Eq, Ord)
instance Show Trait where
    show (Trait name) = name
instance IsString Trait where
    fromString = Trait

data Scheme =
    [(TVar, [Trait])] :=> Type
    deriving (Eq)
infixr 8 :=>
instance Show Scheme where
    show (constraints :=> ty) =
        let vars = do
             (var, traits) <- constraints
             if null traits
                 then return $ show var
                 else return $ show var <> ": " <> (traits |> map show |> intercalate " + ")
        in if null vars then "" else "<" <> (vars |> intercalate ", ") <> ">" <> show ty
instance Substitutable Scheme where
    applySubst (Subst subst) (cs :=> ty) =
        let cs' = cs |> map (\(tv, cs) -> (TVar tv, cs)) |> Map.fromList
            subst' = Subst (subst `Map.difference` cs')
        in cs :=> applySubst subst' ty
    freeTypeVars (cs :=> ty) =
        Set.difference (freeTypeVars ty) (cs |> map fst |> Set.fromList)

data Constraint =
    Matches Type Type
    | Implements Type [Trait]
    deriving (Show, Eq)
instance Substitutable Constraint where
    applySubst s = \case
        Matches t1 t2 -> Matches (applySubst s t1) (applySubst s t2)
        Implements t1 traits -> Implements (applySubst s t1) traits
    freeTypeVars = \case
        Matches t1 t2 -> freeTypeVars t1 <> freeTypeVars t2
isImplConstraint :: Constraint -> Bool
isImplConstraint (Implements _ _) = True
isImplConstraint _ = False

newtype Subst =
    Subst (Map.Map Type Type)
    deriving (Show, Eq)
instance Substitutable Subst where
    applySubst s (Subst target) = Subst (fmap (applySubst s) target)
    freeTypeVars (Subst tvars) = tvars |> Map.keys
                                       |> filter isTVar
                                       |> map (\(TVar tv) -> tv)
                                       |> Set.fromList
instance Semigroup Subst where
    subst1 <> subst2 = Subst (s1 <> s2)
        where
            Subst s1 = subst1
            Subst s2 = applySubst subst1 subst2
instance Monoid Subst where
    mappend = (<>)
    mempty = Subst Map.empty

getSubst :: Type -> Subst -> Maybe Type
getSubst tv (Subst m) = Map.lookup tv m

class Substitutable a where
    applySubst :: Subst -> a -> a
    freeTypeVars :: a -> Set.Set TVar

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    applySubst s (x, y) = (applySubst s x, applySubst s y)
    freeTypeVars (x, y) = freeTypeVars x <> freeTypeVars y

instance Substitutable a => Substitutable [a] where
    applySubst s = map (applySubst s)
    freeTypeVars = Set.unions . map freeTypeVars

traitsTable :: Map.Map Trait ([TCon], TCon)
traitsTable = Map.fromList [ ("Num",        (["integer", "double"], "integer")),
                             ("Integral",   (["integer"], "integer")),
                             ("Fractional", (["double"], "double")),
                             ("Eq",         (["integer", "double", "bool"], "integer")),
                             ("Ord",        (["integer", "double"], "integer")),
                             ("Default",    (["integer", "double", "bool"], "integer"))]

builtinBinaryOps :: [(Name, Scheme)]
builtinBinaryOps = [
        ( "+", [("T", ["Num"])] :=> ["T", "T"] :-> "T"),
        ( "-", [("T", ["Num"])] :=> ["T", "T"] :-> "T"),
        ( "*", [("T", ["Num"])] :=> ["T", "T"] :-> "T"),
        ( "/", [("T", ["Num"])] :=> ["T", "T"] :-> "T"),
        ( "%", [("T", ["Num"])] :=> ["T", "T"] :-> "T"),
        ( "<", [("T", ["Ord"])] :=> ["T", "T"] :-> int),
        ( ">", [("T", ["Ord"])] :=> ["T", "T"] :-> int),
        ("==", [("T", ["Eq"])] :=> ["T", "T"] :-> int),
        ("!=", [("T", ["Eq"])] :=> ["T", "T"] :-> int),
        ( ":", [("T", []), ("U", [])] :=> ["T", "U"] :-> "U")
    ]

builtinUnaryOps :: [(Name, Scheme)]
builtinUnaryOps = [
        ("!", [("T", ["Num"])] :=> ["T"] :-> int),
        ("-", [("T", ["Num"])] :=> ["T"] :-> "T")
    ]

builtinFunctions :: [(Name, Scheme)]
builtinFunctions = [
        ("default", [("T", ["Default"])] :=> [] :-> "T")
    ]
