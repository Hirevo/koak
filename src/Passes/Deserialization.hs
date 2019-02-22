{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Passes.Deserialization where

import Annotation
import Errors
import Misc
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.List (intercalate)

import Types as T

import qualified Data.Map as Map
import qualified Parser.Lib as L
import qualified Parser.Lang as P

deserializeArg :: Ann a P.Arg -> String
deserializeArg (Ann _ (P.Arg name ty)) =
    error "Not implemented (Arg)"

deserializeStmt :: Ann a (P.Stmt a) -> String
deserializeStmt = \case
    Ann _ (P.Defn defn_ty name args ret_ty body) ->
        error "Not implemented (Defn)"
    Ann _ (P.Expr expr) ->
        error "Not implemented (Expr)"
    Ann _ (P.Extern name args ret_ty) ->
        error "Not implemented (Extern)"

deserializeExpr :: Ann a (P.Expr a) -> String
deserializeExpr = \case
    Ann _ (P.For init cond oper body) ->
        error "Not implemented (For)"
    Ann _ (P.If cond then_body else_body) ->
        error "Not implemented (If)"
    Ann _ (P.While cond body) ->
        error "Not implemented (While)"
    Ann _ (P.Call (Ann _ name) args) ->
        error "Not implemented (Call)"
    Ann _ (P.Bin (Ann _ name) lhs rhs) ->
        error "Not implemented (Bin)"
    Ann _ (P.Un (Ann _ name) rhs) ->
        error "Not implemented (Un)"
    Ann _ (P.Ident ident) ->
        error "Not implemented (Ident)"
    Ann _ (P.Lit lit@(P.IntLiteral value)) ->
        error "Not implemented (IntLiteral)"
    Ann _ (P.Lit lit@(P.DoubleLiteral value)) ->
        error "Not implemented (DoubleLiteral)"
    Ann _ (P.Lit lit@(P.BooleanLiteral value)) ->
        error "Not implemented (BooleanLiteral)"
    Ann _ (P.Lit lit@P.VoidLiteral) ->
        error "Not implemented (VoidLiteral)"

deserializeAST :: P.AST a -> String
deserializeAST stmts = stmts |> map deserializeStmt
                             |> intercalate "\n\n"
