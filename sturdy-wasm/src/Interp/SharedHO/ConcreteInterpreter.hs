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

data Interrupt = Error String | Branching Int | Returning deriving (Eq, Show)

newtype Concrete a = Concrete
    { runConcrete :: ExceptT Interrupt (ReaderT (M.Map String (Concrete ()))
        (State (ToyState Value))) a }
    deriving (Functor, Applicative, Monad, MonadState (ToyState Value),
        MonadError Interrupt, MonadReader (M.Map String (Concrete ())))

instance Interp Concrete Value where
    pushBlock _ _ br adv = do
        st1 <- get
        put $ set stack [] st1
        catchError adv $ \e -> case e of
            Branching n -> if n <= 0
                then br
                else throwError $ Branching $ n - 1
            Error msg   -> throwError $ Error msg
            Returning   -> throwError $ Returning
        st2 <- get
        v <- pop
        put $ set stack (v : view stack st1) st2

    popBlock n = throwError $ Branching n

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
            Nothing -> throwError $ Error $ "Var " ++ var ++ " not in scope."

    push v = modify $ over stack (v:)

    pop = do
        st <- get
        case view stack st of
            v:_ -> do
                modify $ over stack tail
                return v
            []  -> throwError $ Error $ "Tried to pop value from empty stack."

    assignFunc name f m = local (M.insert name f) m

    call name = do
        fs <- ask
        case M.lookup name fs of
            Just f  -> f
            Nothing -> throwError $ Error $ "No func " ++ name ++ " in module."

    closure m = do
        st <- get
        put emptyToySt
        catchError m $ \e -> case e of
            Returning   -> return ()
            Branching _ -> throwError $ Error $ "Func-level branch."
            Error msg   -> throwError $ Error msg
        v <- pop
        put st
        push v

    return_ = throwError Returning

instance Fix (Concrete ()) where
    fix f = f (fix f)

run :: Expr -> (Either Interrupt (), ToyState Value)
run e = runState
            (runReaderT
                (runExceptT
                    (runConcrete
                        ((interp :: Expr -> Concrete ()) e))) M.empty) emptyToySt

runFunc :: ToyModule -> String -> [Value] -> (Either Interrupt (), ToyState Value)
runFunc mdl name vs =
    runState
        (runReaderT
            (runExceptT
                (runConcrete
                    ((interpFunc :: ToyModule -> String -> [Value] -> Concrete ()) mdl name vs)))
                        M.empty) emptyToySt
