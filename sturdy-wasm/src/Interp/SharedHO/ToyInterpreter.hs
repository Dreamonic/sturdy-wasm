{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interp.SharedHO.ToyInterpreter
() where

import Prelude hiding (seq, const)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fail
import Control.Monad.Except

data Expr
    = Branch Int
    | Block Expr
    | Seq Expr Expr
    | Const Int
    | Add Expr Expr
    | If Expr Expr Expr

interp :: (Interp m a, MonadError String m) => Expr -> m a
interp e = case e of
    Block e -> push (interp e)

    Branch n -> pop n

    Seq e1 e2 -> snd <$> seq (interp e1) (interp e2)

    Const n -> const n

    Add e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        add v1 v2

    If e t f -> if_ (interp e) (interp t) (interp f)

-- | Interface over Monad m and value type a that allows interpretation
class (Monad m) => Interp m a where
    push :: m a -> m a
    pop :: Int -> m a
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
    push f = do
        state <- get
        put (f:state)
        f

    pop n = do
        state <- get
        let state' = drop n state
        if null state' then
            throwError $ "Can't break " ++ (show n)
        else do
            put (drop 1 state')
            head state'

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

newtype Checker a = Checker { runChecker :: (ReaderT Int (ExceptT String Identity) a) }
    deriving (Functor, Applicative, Monad, MonadReader Int, MonadError String)

instance Interp Checker () where
    push f = local (+1) f

    pop n = do
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
