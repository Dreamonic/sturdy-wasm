{-# LANGUAGE Arrows #-}

module Control.Arrow.Chain
    ( chainN
    , mapA
    , mapA_
    , doN
    , doN_
    , doWhile
    , doWhile_
    ) where

import Control.Arrow

chainN :: ArrowChoice c => c a a -> c (a, Int) a
chainN f = proc (x, n) -> if n <= 0
    then returnA -< x
    else do
        y <- f -< x
        chainN f -< (y, n - 1)

mapA :: ArrowChoice c => c a b -> c [a] [b]
mapA f = proc list -> case list of
    []   -> returnA -< []
    x:xs -> do
        y <- f -< x
        ys <- mapA f -< xs
        returnA -< y:ys

mapA_ :: ArrowChoice c => c a b -> c [a] ()
mapA_ f = mapA f >>^ (\_ -> ())

doN :: ArrowChoice c => c a b -> c (a, Int) [b]
doN f = proc (x, n) -> do
    mapA f -< replicate n x

doN_ :: ArrowChoice c => c a b -> c (a, Int) ()
doN_ f = doN f >>^ (\_ -> ())

doWhile :: ArrowChoice c => c a Bool -> c x y -> c (a, x) [y]
doWhile b f = proc (a, x) -> do
    continue <- b -< a
    if continue
        then do
            y <- f -< x
            ys <- doWhile b f -< (a, x)
            returnA -< y:ys
        else returnA -< []

doWhile_ :: ArrowChoice c => c a Bool -> c x () -> c (a, x) ()
doWhile_ b f = doWhile b f >>^ (\_ -> ())
