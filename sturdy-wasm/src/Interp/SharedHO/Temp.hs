{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Temp where

import Control.Monad.State
import Control.Monad.Except
import Data.Sequence as S
import System.Clock

type M = ExceptT String (State Int)

runM :: M a -> (Either String a, Int)
runM m = runState (runExceptT m) 0

program :: M ()
program = do
    modify (+1)
    catchError (do { modify (+2); throwError "woops" }) (\_ -> return ())
    modify (+4)

type M2 = StateT Int (Except String)

runM2 :: M2 a -> Either String (a, Int)
runM2 m = runExcept (runStateT m 0)

program2 :: M2 ()
program2 = do
    modify (+1)
    catchError (do { modify (+2); throwError "woops" }) (\_ -> return ())
    modify (+4)

data Tree a = Node a (Tree a) (Tree a) | Leaf

-- programTree =    ()
--               /     \
--            (+1)       ()
--            / \      /   \
--         (+2) leaf loop  (+4)
--         /  \            /  \
--      loop  leaf      loop  leaf

type C a = State Int a

programTree :: Tree (C ())
programTree =
    Node (return ())
        (Node (modify (+1)) (Node (modify (+2)) programTree Leaf) Leaf)
            (Node (return ()) programTree (Node (modify (+4)) programTree Leaf))

-- program that finds the first occurrence of a certain int in the state
valOccur :: Tree (C ()) -> Int -> Int
valOccur tree v = valOccur' v $ singleton (return (), tree, 0)

valOccur' :: Int -> Seq (C (), Tree (C ()), Int) -> Int
valOccur' v queue = case queue of
    (c, tree, d) :<| rest -> case tree of
        Node cNew left right -> valOccur' v $
            rest :|> (c >> cNew, left, d + 1) :|> (c >> cNew, right, d + 1)
        Leaf -> if (execState c 0) == v
            then d
            else valOccur' v rest
    Empty -> (-1)

valOccur2 :: Tree (C ()) -> Int -> Int
valOccur2 tree v = valOccur2' v $ singleton (0, tree, 0)

valOccur2' :: Int -> Seq (Int, Tree (C ()), Int) -> Int
valOccur2' v queue = case queue of
    (st, tree, d) :<| rest -> case tree of
        Node c left right -> valOccur2' v $
            rest :|> (execState c st, left, d + 1)
                 :|> (execState c st, right, d + 1)
        Leaf -> if st == v
            then d
            else valOccur2' v rest
    Empty -> (-1)

stopwatch :: Show a => a -> IO ()
stopwatch c = do
    spec1 <- getTime Monotonic
    putStrLn $ show c
    spec2 <- getTime Monotonic
    putStrLn $ show $ nsec spec2 - nsec spec1

fixfix :: (a -> a) -> a
fixfix f = f (fixfix f)

fact :: (((Int -> Int) -> (Int -> Int)) -> (Int -> Int)) -> Int -> Int
fact fix = (fix $ \fact' n -> if n <= 0 then 1 else n * (fact' $ n - 1))

-- idea
-- when we have some computation:
--  f = do {g; h;}
-- then we want the computation to still terminate even if g or h are
-- potentially infinite.

-- class BoundedDomain a where
--     top :: a -> Bool
--     bottom :: a -> Bool
--
-- class MonadState s m => MonadTerminate s m where
--     haltWhen :: (s -> Bool) -> m a -> m a
--
-- data Terminate a = Terminate
