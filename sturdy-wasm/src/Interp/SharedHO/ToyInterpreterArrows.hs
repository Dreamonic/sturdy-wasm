{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}

{-# LANGUAGE BangPatterns #-}

module Interp.SharedHO.ToyInterpreterArrows
() where

import Prelude hiding (seq, const, (.), fail, id)
import Data.String
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Concrete.Error

import Control.Category
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Fail hiding (fail')
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Concrete.Failure

import Debug.Trace

data Expr
    = Branch Int
    | Block Expr Expr
    | Seq Expr Expr
    | Const Int
    | Add Expr Expr
    | If Expr Expr Expr

class (Arrow c, ArrowApply c) => InterpA a c where
    pushBlock :: c (c () a, c () a) a
    popBlock :: c Int (c () a)
    const :: c Int a
    add :: c (a, a) a
    if_ :: c x a -> c y a -> c (a, x, y) a

    seq :: c ((c () a), (c () a)) a
    seq = proc (c1, c2) -> do
        c1 -<< ()
        c2 -<< ()

interp :: (Arrow c, ArrowChoice c, InterpA a c, ArrowApply c) => c Expr a
interp = proc e -> case e of
    Const n -> const -< n
    Block enterBl afterBl -> do
        pushBlock -< ((app' interp enterBl), (app' interp afterBl))
    Branch n -> do
        bl <- popBlock -< n
        bl -<< ()
    Seq e1 e2 -> do
        seq -< ((app' interp e1), (app' interp e2))
    Add e1 e2 -> do
        v1 <- interp -< e1
        v2 <- interp -< e2
        add -< (v1, v2)
    If c t f -> do
        c' <- interp -< c
        if_ interp interp -< (c', t, f)

newtype Concrete x y = Concrete { 
    runConcrete :: ([Concrete () Int], x) -> ([Concrete () Int], Either String y)
}

instance Arrow Concrete where
    arr f = Concrete $ \(s, x) -> (s, Right $ f x)
    first (Concrete f) = Concrete $ \(s, (b, d)) -> do
        case f (s, b) of
            (_, Left err) -> (s, Left err)
            (s', Right res) -> (s', Right (res, d))

instance Category Concrete where
    id = arr id
    (Concrete g) . (Concrete f) = Concrete $ \(s, x) -> case f (s, x) of
        (_, Left err) -> (s, Left err)
        (s', Right res) -> g (s', res)

instance ArrowChoice Concrete where
    left (Concrete f) = Concrete $ \(s, x) -> case x of
        Left b -> do
            case f (s, b) of
                (_, Left err) -> (s, Left err)
                (s', Right res) -> (s', Right (Left res))
        Right d -> do
            (s, Right (Right d))

instance ArrowApply Concrete where
    app = Concrete $ \(s, (Concrete f, b)) -> f (s, b)

app' :: (Arrow c, ArrowApply c) => c a b -> a -> c () b
app' c a = proc () -> app -< (c, a)

get' :: Concrete () [Concrete () Int]
get' = Concrete $ \(s, _) -> (s, Right s)

put' :: Concrete [Concrete () Int] ()
put' = Concrete $ \(s, s') -> (s', Right ())

fail' :: Concrete String (Concrete () Int)
fail' = Concrete $ \(s, str) -> (s, Left str)

instance InterpA Int Concrete where
    pushBlock = proc (enter, after) -> do
        state <- get' -< ()
        put' -< after:state
        enter -<< ()

    popBlock = proc n -> do
        state <- get' -< ()
        let state' = drop n state
        if null state' then
            fail' -< "Can't break " ++ (show n)
        else do
            put' -< (drop 1 state')
            returnA -< head state'
        
    const = returnA

    add = proc (v1, v2) -> returnA -< v1 + v2

    if_ t f = proc (c, tArg, fArg) -> do
        if c == 0 then
            f -< fArg
        else
            t -< tArg

run' :: Expr -> Either String Int
run' e = do
    let comp = (interp :: Concrete Expr Int)
    let run (Concrete f) e = f ([], e)
    snd $ run comp e

-- newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
--     deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowRun,
--               ArrowState s, ArrowFail e)

-- instance ArrowTrans (ConcreteT c) where type Underlying (ConcreteT c) x y = c x y

-- instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ConcreteT c) where
--   app = lift (app .# first coerce)
--   {-# INLINE app #-}

-- instance (ArrowChoice c, ArrowApply c, ArrowFail String c, ArrowState [ConcreteT c () Int] c) 
--     => InterpA Int (ConcreteT c) where

--     pushBlock = proc bl -> do
--         state <- get -< ()
--         put -< bl:state
--         bl -<< ()

--     popBlock = proc n -> do
--         state <- get -< ()
--         let state' = drop n state
--         if null state' then
--             fail -< "Can't break " ++ (show n)
--         else do
--             put -< (drop 1 state')
--             returnA -< head state'
        
--     const = returnA

-- newtype Concrete x y = Concrete { runConcrete :: (ConcreteT (FailureT String (StateT [Concrete () Int] (->)))) x y }
--     deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowRun,
--               ArrowState [Concrete () Int], ArrowFail String)

-- instance ArrowApply Concrete where
--     app = app
--     {-# INLINE app #-}
              
-- instance InterpA Int Concrete where
--     pushBlock = pushBlock
--     popBlock = popBlock
--     const = const
--     seq = seq

-- run' :: Expr -> Either String Int
-- run' e = do
--     let comp = (interp :: Concrete Expr Int)
--     let result = Trans.run comp ([], e)
--     toEither $ snd result
