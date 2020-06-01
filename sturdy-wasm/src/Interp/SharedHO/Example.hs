{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interp.SharedHO.Example
where

import Control.Monad.Identity

data Expr
    = Lit Int
    | Add Expr Expr

class Monad m => Interp v m where
    lit :: Int -> m v
    add :: v -> v -> m v

interp :: Interp v m => Expr -> m v
interp (Lit n)     = lit n
interp (Add e1 e2) = do x <- interp e1
                        y <- interp e2
                        add x y


-- Concrete instance

newtype Concrete a = Concrete
    { concrete :: Identity a
    } deriving (Functor, Applicative, Monad)

instance Interp Int Concrete where
    lit n   = return n
    add x y = return $ x + y

runConcrete e = runIdentity $ concrete $
    (interp :: Expr -> Concrete Int) e


-- Abstract instance

data Interval a = Interval a a

newtype Abstract a = Abstract
    { abstract :: Identity a
    } deriving (Functor, Applicative, Monad)

instance Interp (Interval Int) Abstract where
    lit n = return $ Interval n n
    add (Interval l1 h1) (Interval l2 h2)
          = return $ Interval (l1 + l2) (h1 + h2)

runAbstract e = runIdentity $ abstract $
  (interp :: Expr -> Abstract (Interval Int)) e
