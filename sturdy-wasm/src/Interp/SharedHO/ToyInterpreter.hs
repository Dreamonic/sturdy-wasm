{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

module Interp.SharedHO.ToyInterpreter
() where

import Prelude hiding (seq, const)
import Data.Abstract.Interval
import Data.Order
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

data Expr
    = Branch Int
    | Block Expr Expr
    | Seq Expr Expr
    | Const Int
    | Add Expr Expr
    | If Expr Expr Expr

interp :: (Interp m a, MonadError String m) => Expr -> m a
interp e = case e of
    Block eE eA -> pushBlock (interp eE) (interp eA)

    Branch n -> popBlock n

    Seq e1 e2 -> snd <$> seq (interp e1) (interp e2)

    Const n -> const n

    Add e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        add v1 v2

    If e t f -> if_ (interp e) (interp t) (interp f)

-- | Interface over Monad m and value type a that allows interpretation
class (Monad m) => Interp m a where
    pushBlock :: m a -> m a -> m a
    popBlock :: Int -> m a
    const :: Int -> m a
    add :: a -> a -> m a
    if_ :: m a -> m a -> m a -> m a

    seq :: m a -> m a -> m (a, a)
    seq m1 m2 = do
        v1 <- m1
        v2 <- m2
        return (v1, v2)

-- Concrete interpreter

newtype Concrete a = Concrete { runConcrete :: StateT [Concrete Int] (ExceptT String Identity) a }
    deriving (Functor, Applicative, Monad, MonadState [Concrete Int], MonadError String)

instance Interp Concrete Int where
    pushBlock enter after = do
        st <- get
        put (after:st)
        enter

    popBlock n = do
        st <- get
        let st' = drop n st
        if null st' then
            throwError $ "Can't break " ++ (show n)
        else do
            put (drop 1 st')
            head st'

    const = return

    add v1 v2 = return (v1 + v2)

    if_ c t f = do
        c' <- c
        if c' == 0 then f else t

run :: Expr -> Either String Int
run e = fst <$> runExcept
            (runStateT
                (runConcrete
                    ((interp :: Expr -> Concrete Int) e)) [])

-- Interpreter that only checks if block structure is correct

newtype Checker a = Checker { runChecker :: ReaderT Int (ExceptT String Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader Int, MonadError String)

instance Interp Checker () where
    pushBlock enter after = do
        local (+1) enter
        after

    popBlock n = do
        level <- ask
        if n >= level then
            throwError $ "Can't break " ++ (show n)
        else
            return ()

    const _ = return ()

    add _ _ = return ()

    if_ c t f = c >> t >> f

check :: Expr -> Either String ()
check e = runExcept
            (runReaderT
                (runChecker
                    ((interp :: Expr -> Checker ()) e)) 0)


newtype IntvlAnalys a = IntvlAnalys
    { runInterval :: StateT [IntvlAnalys (Interval Int)] (ExceptT String Identity) a }
    deriving (Functor, Applicative, Monad, MonadState [IntvlAnalys (Interval Int)], MonadError String)

instance Interp IntvlAnalys (Interval Int) where
    pushBlock enter after = do
        st <- get
        put (after:st)
        enter

    popBlock n = do
        st <- get
        let st' = drop n st
        if null st' then
            throwError $ "Can't break " ++ (show n)
        else do
            put (drop 1 st')
            head st'

    const n = return (constant n)

    add i1 i2 = return (i1 + i2)

    if_ c t f = do
        c' <- c
        let zero = constant 0
        if | zero ≈ c'       -> f
           | not (zero ⊑ c') -> t
           | otherwise       -> do
               vt <- t
               vf <- f
               return (vt ⊔ vf)


runIntvlAnalys :: Expr -> Either String (Interval Int)
runIntvlAnalys e =
    fst <$> runExcept
               (runStateT
                   (runInterval
                       ((interp :: Expr -> IntvlAnalys (Interval Int)) e)) [])
