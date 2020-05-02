{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.ConcreteInterpreter
where

import Prelude hiding (const, lookup)
import qualified Data.Map as M
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Monad.Writer hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import Interp.SharedHO.Data.BoolVal
import Interp.SharedHO.Data.Types
import Interp.SharedHO.Data.ToyState
import Interp.SharedHO.GenericInterpreter

newtype Concrete a = Concrete
    { runConcrete :: ExceptT (Either String Int) (State (ToyState Value)) a }
    deriving (Functor, Applicative, Monad, MonadState (ToyState Value),
        MonadError (Either String Int))

instance Interp Concrete Value where
    pushBlock _ _ br adv = do
        st1 <- get
        put $ set stack [] st1
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg
        st2 <- get
        v <- pop
        put $ set stack (v : view stack st1) st2

    popBlock n = throwError $ Right n

    const = return

    add v1 v2 = return $ v1 + v2

    lt v1 v2 = return $ v1 + v2

    eqz (Value t v) = return $ Value t (fromBool . not . toBool $ v)

    if_ c t f = if toBool c then t else f

    assign var v = do
        st <- get
        put $ over variables (M.insert var v) st

    lookup var = do
        st <- get
        case M.lookup var (view variables st) of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

    push v = modify $ over stack (v:)

    pop = do
        st <- get
        case view stack st of
            v:_ -> do
                modify $ over stack tail
                return v
            []  -> throwError $ Left $ "Tried to pop value from empty stack."

instance Fix (Concrete ()) where
    fix f = f (fix f)

run :: Expr -> (Either (Either String Int) (), ToyState Value)
run e = runState
            (runExceptT
                (runConcrete
                    ((interp :: Expr -> Concrete ()) e))) emptyToySt
