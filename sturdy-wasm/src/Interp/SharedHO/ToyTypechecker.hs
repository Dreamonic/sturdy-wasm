{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Interp.SharedHO.ToyTypechecker
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)

import Interp.SharedHO.Joinable
import Interp.SharedHO.BoolVal

data Type
    = I32
    | I64
    deriving (Show, Eq)

data Value 
    = Value Type Integer
    deriving (Show, Eq)

data Expr
    = Branch Int
    | Block Expr
    | Loop Expr
    | Seq [Expr]
    | Const Value
    | Add Expr Expr
    | Lt Expr Expr
    | If Expr Expr Expr
    | Assign String Expr
    | Var String

instance ToBool Integer where
    toBool = (/=) 0

instance FromBool Integer where
    fromBool b = if b then 1 else 0

class Monad m => Interp m a | m -> a where
    pushBlock :: m a -> m a -> m a
    popBlock :: Int -> m a
    const :: Value -> m a
    add :: a -> a -> m a
    lt :: a -> a -> m a
    if_ :: a -> m a -> m a -> m a
    assign :: String -> a -> m a
    lookup :: String -> m a
    nop :: m a

class Fix c where
    fix :: (c -> c) -> c

interp :: (Interp m a, Fix (m a)) => Expr -> m a
interp expr = case expr of
    Branch n -> popBlock n

    Block e -> pushBlock nop (interp e)

    Loop e -> fix $ \lp -> pushBlock lp (interp e)

    Seq es -> foldl (>>) nop (interp <$> es)

    Const n -> const n

    Add e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        add v1 v2

    Lt e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        lt v1 v2

    If e t f -> do
        c <- interp e
        if_ c (interp t) (interp f)

    Assign var e -> do
        v <- interp e
        assign var v

    Var var -> lookup var

type TypeCheckState = M.Map String Type

newtype TypeChecker a = TypeChecker
    { runTypeChecker :: ExceptT (Either String Int) (State TypeCheckState) a}
    deriving (Functor, Applicative, Monad, MonadError (Either String Int), 
        MonadState TypeCheckState)
