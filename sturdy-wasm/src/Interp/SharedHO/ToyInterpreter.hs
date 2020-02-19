{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interp.SharedHO.ToyInterpreter
() where

import Control.Monad.State

newtype M a = M (State [M ()] a) -- typechecker Reader Int a (niveau)
    deriving (Functor, Applicative, Monad, MonadState [M ()])

data Expr
    = Branch Int
    | Block Expr
    | Seq Expr Expr
    | Nop

interp :: Expr -> M ()
interp Nop = return ()
interp (Seq e1 e2) = do
    interp e1
    interp e2
interp (Block e) = push (interp e)
interp (Branch n) = pop n


push :: (M ()) -> M ()
push f = do
    st <- get
    put (f:st)
    f

pop :: Int -> M ()
pop n = do
    st <- get
    let st' = drop n st
    put (drop 1 st')
    head st'


-- type checker door alleen M en push en pop aan te passen
