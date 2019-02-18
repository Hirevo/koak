{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen.Specialization where
    
import Misc
import Annotation
import Control.Monad.State.Lazy
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import Data.List (partition)
import Data.Maybe (fromJust)

import qualified Parser.Lang as P
import qualified Types as Ty
import qualified Codegen.Prelude as Pre
import qualified Codegen.Utils as U
import qualified Data.Map as Map

type Specialized = State (Map.Map Ty.TVar Ty.Type)
class Specialize a where
    specialize :: a Ty.Type -> Specialized (a Ty.Type)

instance Specialize P.Expr where
    specialize (P.For ty start cond oper body) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        start_s <- specialize start
        cond_s <- specialize cond
        oper_s <- specialize oper
        body_s <- specialize body
        return (P.For new_ty start_s cond_s oper_s body_s)
    specialize (P.If ty cond then_body else_block) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        cond_s <- specialize cond
        then_body_s <- specialize then_body
        else_block_s <- sequence $ fmap specialize else_block
        return (P.If new_ty cond_s then_body_s else_block_s)
    specialize (P.While ty cond body) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        cond_s <- specialize cond
        body_s <- specialize body
        return (P.While new_ty cond_s body_s)
    specialize (P.Call ty name args) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        args_s <- forM args specialize
        return (P.Call new_ty name args_s)
    specialize (P.Bin ty (Ann tmp "=") name start) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        name_s <- specialize name
        start_s <- specialize start
        return (P.Bin new_ty (Ann tmp "=") name_s start_s)
    specialize (P.Bin ty name lhs rhs) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        lhs_s <- specialize lhs
        rhs_s <- specialize rhs
        return (P.Bin new_ty name lhs_s rhs_s)
    specialize (P.Un ty name rhs) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        rhs_s <- specialize rhs
        return (P.Un new_ty name rhs_s)
    specialize (P.Ident ty name) = do
        new_ty <- case ty of
            Ty.TVar v -> gets $ \env -> fromJust $ Map.lookup v env
            _ -> return ty
        return (P.Ident new_ty name)
    specialize el = return el
