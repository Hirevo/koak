{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Codegen.Specialization where
    
import Misc
import Annotation
import Control.Monad.State.Lazy

import Data.List (partition)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import qualified Parser.Lib as L
import qualified Parser.Lang as P
import qualified Types as Ty
import qualified Codegen.Prelude as Pre
import qualified Codegen.Utils as U
import qualified Data.Map as Map

type Specialize = State (Map.Map Ty.TVar Ty.Type)

specializeExpr :: Ann (L.Range, Ty.Type) (P.Expr (L.Range, Ty.Type)) -> Specialize (Ann (L.Range, Ty.Type) (P.Expr (L.Range, Ty.Type)))
specializeExpr = \case
    Ann (range, ty) (P.For start cond oper body) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        start_s <- specializeExpr start
        cond_s <- specializeExpr cond
        oper_s <- specializeExpr oper
        body_s <- specializeExpr body
        return (Ann (range, new_ty) $ P.For start_s cond_s oper_s body_s)
    Ann (range, ty) (P.If cond then_body else_block) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        cond_s <- specializeExpr cond
        then_body_s <- specializeExpr then_body
        else_block_s <- sequence $ fmap specializeExpr else_block
        return (Ann (range, new_ty) $ P.If cond_s then_body_s else_block_s)
    Ann (range, ty) (P.While cond body) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        cond_s <- specializeExpr cond
        body_s <- specializeExpr body
        return (Ann (range, new_ty) $ P.While cond_s body_s)
    Ann (range, ty) (P.Call name args) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        args_s <- forM args specializeExpr
        return (Ann (range, new_ty) $ P.Call name args_s)
    Ann (range, ty) (P.Bin (Ann tmp "=") name start) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        name_s <- specializeExpr name
        start_s <- specializeExpr start
        return (Ann (range, new_ty) $ P.Bin (Ann tmp "=") name_s start_s)
    Ann (range, ty) (P.Bin name lhs rhs) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        lhs_s <- specializeExpr lhs
        rhs_s <- specializeExpr rhs
        return (Ann (range, new_ty) $ P.Bin name lhs_s rhs_s)
    Ann (range, ty) (P.Un name rhs) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        rhs_s <- specializeExpr rhs
        return (Ann (range, new_ty) $ P.Un name rhs_s)
    Ann (range, ty) (P.Ident name) -> do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        return (Ann (range, new_ty) $ P.Ident name)
    rest -> return rest
