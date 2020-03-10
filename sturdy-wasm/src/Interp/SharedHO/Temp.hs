{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Interp.SharedHO.Temp where

import Control.Monad.State
import Control.Monad.Except

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
