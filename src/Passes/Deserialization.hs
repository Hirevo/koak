{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Passes.Deserialization where

import Annotation
import Misc

import Data.List (intercalate)

import qualified Parser.Lang as P

deserializeArg :: Ann a P.Arg -> String
deserializeArg (Ann _ (P.Arg name ty)) =
    name ++ ":" ++ show ty

deserializeStmt :: Ann a (P.Stmt a) -> String
deserializeStmt = \case
    Ann _ (P.Defn defn_ty name args ret_ty body) ->
        "def " ++ 
            case defn_ty of
                P.Binary (P.LeftAssoc value) -> "binary " ++ name ++ " " ++ show value ++ " left "
                P.Binary (P.RightAssoc value) -> "binary " ++ name ++ " " ++ show value ++ " right "
                P.Unary value -> "unary " ++ name ++ " " ++ show value ++ " "
                _ -> name
            ++ "(" ++ (args |> map deserializeArg |> intercalate " ") ++ "): " ++ show ret_ty ++ "\n\t" ++ deserializeExpr body ++ ";"
    Ann _ (P.Expr expr) ->
        deserializeExpr expr ++ ";"
    Ann _ (P.Extern name args ret_ty) ->
        "extern " ++ name ++ "(" ++ (args |> map deserializeArg |> intercalate " ") ++ "): " ++ show ret_ty ++ ";"
    --Ann _ (P.Struct name args) ->
        --"struct " ++ name ++ (args |> map deserializeArg |> intercalate " ") ++ ";"

deserializeExpr :: Ann a (P.Expr a) -> String
deserializeExpr = \case
    Ann _ (P.Lambda args body) ->
        --TODO: Show args ?
        "(" ++ " -> " ++ deserializeExpr body ++ ")"
    Ann _ (P.For init cond oper body) ->
        "(for " ++ deserializeExpr init ++ ", " ++ deserializeExpr cond ++ ", " ++ deserializeExpr oper ++ " in\n\t" ++ deserializeExpr body ++ ")"
    Ann _ (P.If cond then_body else_body) ->
            "(if " ++ deserializeExpr cond ++ " then " ++ deserializeExpr then_body ++
                case else_body of
                    Just else_body ->
                        " else " ++ deserializeExpr else_body
                    _ ->
                        "if " ++ deserializeExpr cond ++ " then " ++ deserializeExpr then_body
                ++ ")"
    Ann _ (P.While cond body) ->
        "(while " ++ deserializeExpr cond ++ " do " ++ deserializeExpr body ++ ")"
    Ann _ (P.Call (Ann _ name) args) ->
        name ++ "(" ++ (args |> map deserializeExpr |> intercalate ", ") ++ ")"
    Ann _ (P.Bin (Ann _ name) lhs rhs) ->
        deserializeExpr lhs ++ " " ++ name ++ " " ++ deserializeExpr rhs
    Ann _ (P.Un (Ann _ name) rhs) ->
        name ++ deserializeExpr rhs
    Ann _ (P.Ident ident) ->
        ident
    Ann _ (P.Lit lit@(P.IntLiteral value)) ->
        show value
    Ann _ (P.Lit lit@(P.DoubleLiteral value)) ->
        show value
    Ann _ (P.Lit lit@(P.BooleanLiteral value)) ->
        show value
    Ann _ (P.Lit lit@P.VoidLiteral) ->
        ""

deserializeAST :: P.AST a -> String
deserializeAST stmts = stmts |> map deserializeStmt
                             |> intercalate "\n\n"
