{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}

module Interp.SharedHO.ToyInterpreterArrows
() where

import Prelude hiding (seq, const, (.), fail)
import Data.String
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))

import Control.Category
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Concrete.Failure

data Expr
    = Branch Int
    | Block Expr
    | Seq Expr Expr
    | Const Int
    | Add Expr Expr
    | If Expr Expr Expr

class (Arrow c) => InterpA a c where
    pushBlock :: c (c () a) a
    popBlock :: c Int (c () a)
    const :: c Int a

    seq :: c () a -> c () a -> c () a
    seq c1 c2 = proc () -> do
        c1 -< ()
        c2 -< ()

interp :: (Arrow c, ArrowChoice c, InterpA Int c) => c Expr Int
interp = proc e -> case e of
    Const n -> const -< n
    Block e -> do
        let a = proc () -> interp -< e
        pushBlock -< a

newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, 
              ArrowState s, ArrowFail e)

instance ArrowTrans (ConcreteT c) where type Underlying (ConcreteT c) x y = c x y

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ConcreteT c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (ArrowChoice c, ArrowApply c, ArrowFail String c, ArrowState [ConcreteT c () Int] c) 
    => InterpA Int (ConcreteT c) where

    pushBlock = proc bl -> do
        state <- get -< ()
        put -< bl:state
        bl -<< ()

    popBlock = proc n -> do
        state <- get -< ()
        let state' = drop n state
        if null state' then
            fail -< "Can't break " ++ (show n)
        else do
            put -< (drop 1 state')
            returnA -< head state'
        
    const = returnA

newtype Concrete x y = Concrete { runConcrete :: (ConcreteT (FailureT String (StateT [Concrete () Int] (->)))) x y }
    deriving (Profunctor, Category, Arrow, ArrowChoice, 
              ArrowState [Concrete () Int], ArrowFail String)

-- (FailureT String (StateT [Concrete c x y] c))

-- run = do
--     runStateT
--         (runFailureT
--             (runConcreteT
--                 (runConcrete (interp :: Concrete Expr Int))))
    -- Control.Arrow.Trans.run comp
