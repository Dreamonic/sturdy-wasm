{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interp.SharedHO.ToyInterpreter
() where

import Control.Monad.State

class Monad m => GenericInterp m where
    push :: m () -> m ()
    pop :: Int -> m ()

data Expr
    = Branch Int
    | Block Expr
    | Seq Expr Expr
    | Nop

interp :: GenericInterp m => Expr -> m ()
interp e = case e of
    Nop       -> return ()

    Seq e1 e2 -> do
        interp e1
        interp e2

    Block e   -> push (interp e)

    Branch n  -> pop n


newtype Concrete a = Concrete (State [Concrete ()] a) -- typechecker Reader Int a (niveau)
    deriving (Functor, Applicative, Monad, MonadState [Concrete ()])

instance GenericInterp Concrete where
    push f = do
        st <- get
        put (f:st)
        f

    pop n = do
        st <- get
        let st' = drop n st
        put (drop 1 st')
        head st'


-- type checker door alleen M en push en pop aan te passen
