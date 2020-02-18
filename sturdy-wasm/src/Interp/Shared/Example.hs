{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interp.Shared.Example where

import Prelude hiding (compare, const, id, fail)
import Data.Profunctor
import Data.String
import Data.Concrete.Error
import Control.Category hiding ((.))
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Concrete.Failure

import Control.Arrow.Wasm

------------- Continue Transformer definition

class (Arrow c, Profunctor c, Arrow comp, Profunctor comp) => ArrowContinue comp c | c -> comp where

    -- | Executes the stored arrow if there is one
    popArrow :: c () ((comp () ()))

    -- | Store an arrow for later execution
    pushArrow :: c (comp () ()) ()


newtype ContinueT comp a b c x y = ContinueT { runContinue :: (StateT [comp a b] c) x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, 
                ArrowRun, ArrowFail e, ArrowState [comp a b])

instance (Arrow c, ArrowChoice c, Profunctor c, Arrow comp, Profunctor comp, ArrowFail e c, IsString e) 
    => ArrowContinue comp (ContinueT comp () () c) where

    popArrow = proc () -> do
        s <- get -< ()
        case s of
            [] -> fail -< "Empty arrow stack"
            a:as -> do
                put -< as
                returnA -< a
    
    pushArrow = proc comp' -> do
        s <- get -< ()
        put -< comp':s


------------ Interpreter

interp :: (ArrowContinue comp c, ArrowFail e c, ArrowFail e comp, IsString e) => c () ()
interp = do
    -- let boundArr = proc () -> do
        -- pushArrow -< fooArrow
        -- popArrow -< ()

    proc () -> do
        -- boundArr -< ()
        returnA -< ()

-- if_ :: (ArrowContinue c c) => c x z -> c y z -> c (v, x, y) z
-- if_ f g = proc (ty, x, y) -> do
--     state <- get -< ()
--     f -< x
--     shouldExit -< True

--     store -< proc () -> do
--         put -< state

-- -- onExit :: (ArrowContinue c c) => 

fooArrow :: (ArrowFail e c, IsString e) => c () ()
fooArrow = proc () -> do
    fail -< "foo"

newtype TestT c x y = TestT {runTest :: c x y} deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e,
    ArrowState s, ArrowWasm v, ArrowContinue interp)

deriving instance ArrowFix (c x y) => ArrowFix (TestT c x y)

instance ArrowRun c => ArrowRun (TestT c) where
    type Run (TestT c) x y = Run c x y
    run = Trans.run . runTest

type Lower = FailureT String (->)

-- test :: Either String ()
-- test = do
--     let comp = proc () -> do
--         (interp :: ContinueT Lower () () Lower () ()) -< ()
--     toEither $ snd <$> Trans.run comp ([], ())