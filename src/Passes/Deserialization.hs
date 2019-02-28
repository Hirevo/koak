{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

newtype Deserialize a = Deserialize {
    runDeserialize :: State Int a
} deriving (Applicative, Functor, Monad, MonadState Int)
(|->) :: MonadState Int m => m a -> m a
(|->) action = do
    modify (+4)
    ret <- action
    modify $ \i -> i - 4
    return ret
(<-|) :: MonadState Int m => m a -> m a
(<-|) action = do
    modify (+4)
    ret <- action
    modify $ \i -> i - 4
    return ret

deserializeArg :: Ann a P.Arg -> String
deserializeArg (Ann _ (P.Arg name ty)) =
    name <> ":" <> show ty

deserializeStmt :: Ann a (P.Stmt a) -> Deserialize String
deserializeStmt = \case
    Ann _ (P.Defn P.Function name args ret_ty body) -> do
        indented <- (|->) (do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> ibody)
        return ("def " <> name <> "(" <> intercalate " " (map deserializeArg args) <> "): " <> show ret_ty <> "\n"
            <> indented <> ";")
    Ann _ (P.Defn (P.Unary pred) name args ret_ty body) -> do
        indented <- (|->) (do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> ibody)
        return $ "def unary " <> name <> " " <> show pred
            <> " (" <> intercalate " " (map deserializeArg args) <> "): " <> show ret_ty <> "\n"
            <> indented <> ";"
    Ann _ (P.Defn (P.Binary (P.LeftAssoc pred)) name args ret_ty body) -> do
        indented <- (|->) (do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> ibody)
        return $ "def binary " <> name <> " " <> show pred
            <> " left (" <> intercalate " " (map deserializeArg args) <> "): " <> show ret_ty <> "\n"
            <> indented <> ";"
    Ann _ (P.Defn (P.Binary (P.RightAssoc pred)) name args ret_ty body) -> do
        indented <- (|->) (do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> ibody)
        return $ "def binary " <> name <> " " <> show pred
            <> " right (" <> intercalate " " (map deserializeArg args) <> "): " <> show ret_ty <> "\n"
            <> indented <> ";"
    Ann _ (P.Expr expr) -> do
        ident <- gets $ flip replicate ' '
        body <- deserializeExpr expr
        return $ ident <> body <> ";"
    Ann _ (P.Extern name args ret_ty) ->
        return $ "extern " <> name <> "(" <> intercalate " " (map deserializeArg args) <> "): " <> show ret_ty <> ";"

deserializeExpr :: Ann a (P.Expr a) -> Deserialize String
deserializeExpr = \case
    Ann _ (P.For init cond oper body) -> do
        indented <- (|->) $ do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> "in " <> ibody
        prelude <- mapM deserializeExpr [init, cond, oper]
        return $ "(for " <> intercalate ", " prelude <> "\n" <> indented <> ")"
    Ann _ (P.If cond then_body else_body) -> do
        icond <- deserializeExpr cond
        ithen <- (|->) $ do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr then_body
            return $ ident <> "then " <> ibody
        ielse <- case else_body of
            Just body -> (|->) (do
                ident <- gets $ flip replicate ' '
                ibody <- deserializeExpr body
                return $ "\n" <> ident <> "else " <> ibody)
            Nothing -> return ""
        return $ "(if " <> icond <> "\n" <> ithen <> ielse <> ")"
    Ann _ (P.While cond body) -> do
        icond <- deserializeExpr cond
        ibody <- (|->) $ do
            ident <- gets $ flip replicate ' '
            ibody <- deserializeExpr body
            return $ ident <> "do " <> ibody
        return $ "(while " <> icond <> "\n" <> ibody <> ")"
    Ann _ (P.Call (Ann _ name) args) -> do
        iargs <- mapM deserializeExpr args
        return $ name <> "(" <> intercalate ", " iargs <> ")"
    Ann _ (P.Bin (Ann _ name) lhs rhs) -> do
        ilhs <- deserializeExpr lhs
        irhs <- deserializeExpr rhs
        return $ intercalate " " [ilhs, name, irhs]
    Ann _ (P.Un (Ann _ name) rhs) -> do
        irhs <- deserializeExpr rhs
        return $ concat [name, irhs]
    Ann _ (P.Ident ident) -> return ident
    Ann _ (P.Lit (P.IntLiteral value)) -> return $ show value
    Ann _ (P.Lit (P.DoubleLiteral value)) -> return $ show value
    Ann _ (P.Lit (P.BooleanLiteral value)) -> return $ if value then "true" else "false"
    Ann _ (P.Lit P.VoidLiteral) -> return "()"

deserializeAST :: P.AST a -> String
deserializeAST stmts = stmts |> mapM deserializeStmt
                             |> runDeserialize
                             |> flip evalState 0
                             |> intercalate "\n\n"
