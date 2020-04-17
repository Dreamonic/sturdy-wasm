{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.Typed.GenericInterpreter
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Data.List (intercalate)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Monad.Writer hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import Interp.SharedHO.Joinable
import Interp.SharedHO.BoolVal
import Interp.SharedHO.Typed.Types

data Expr
    = Branch Int
    | Block Type Expr
    | Loop Type Expr
    | Seq [Expr]
    | Const Value
    | Add
    | Lt
    | Eqz
    | If Type Expr Expr
    | Assign String
    | Var String
    | Nop

instance ToBool Integer where
    toBool = (/=) 0

instance FromBool Integer where
    fromBool b = if b then 1 else 0

class Monad m => Interp m v | m -> v where
    pushBlock :: Type -> Bool -> m () -> m () -> m ()
    popBlock :: Int -> m ()
    const :: Value -> m v
    add :: v -> v -> m v
    lt :: v -> v -> m v
    eqz :: v -> m v
    if_ :: v -> m () -> m () -> m ()
    assign :: String -> v -> m ()
    lookup :: String -> m v
    push :: v -> m ()
    pop :: m v

class Fix c where
    fix :: (c -> c) -> c

interp :: (Interp m a, Fix (m ())) => Expr -> m ()
interp expr = case expr of
    Branch n -> popBlock n

    Block rty e -> pushBlock rty False (return ()) (interp e)

    Loop rty e -> fix $ \br -> pushBlock rty True br (interp e)

    Seq es -> sequence_ (interp <$> es)

    Const n -> do
        v <- const n
        push v

    Add -> do
        v1 <- pop
        v2 <- pop
        v3 <- add v2 v1
        push v3

    Lt -> do
        v1 <- pop
        v2 <- pop
        v3 <- lt v2 v1
        push v3

    Eqz -> do
        v1 <- pop
        v2 <- eqz v1
        push v2

    If rty t f -> do
        c <- pop
        let t' = pushBlock rty False (return ()) (interp t)
        let f' = pushBlock rty False (return ()) (interp f)
        if_ c t' f'

    Assign var -> do
        v <- pop
        assign var v

    Var var -> do
        v <- lookup var
        push v

    Nop -> return ()