{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.ReachingDefinitions
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Lens hiding (Const, assign)

import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.Data.Joinable
import qualified Interp.SharedHO.Data.RDSet as RD
import Interp.SharedHO.Data.ToyState
import Interp.SharedHO.Data.Types
import Interp.SharedHO.Data.BoolVal
import Interp.SharedHO.Data.Interrupt

type ReachDefState = ToyState (RD.Set Value)

newtype ReachDef a = ReachDef
    { runReachDef :: ExceptT Interrupt (ReaderT (M.Map String (ReachDef ()))
        (State ReachDefState)) a }
    deriving (Functor, Applicative, Monad, MonadState ReachDefState,
        MonadReader (M.Map String (ReachDef ())), MonadError Interrupt)

instance Joinable a => Joinable (ReachDef a) where
    join f g = do
        st <- get
        x <- f
        st1 <- get
        put st
        y <- g
        st2 <- get
        put $ st1 `join` st2
        return $ x `join` y

instance Interp ReachDef (RD.Set Value) where
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
        put $ set stack (v : (view stack st1)) st2

    popBlock n = throwError $ Branching n

    const v = return $ RD.singleton v

    add v1 v2 = return $ RD.add v1 v2

    eqz = return . RD.eqz

    lt v1 v2 = return $ RD.lt v1 v2

    if_ c t f = RD.if_ c t f

    assign var v = do
        modify $ over variables (M.insert var v)

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

    assignFunc name f =  local (M.insert name f)

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

instance Fix ReachDefState ReachDef where
    fix img f = do
        st <- get
        let st' = st `join` img
        put st'
        if st' == img
            then push RD.Top
            else f $ fix st' f

runRD :: Expr -> (Either Interrupt (), ReachDefState)
runRD e = runState
              (runReaderT
                  (runExceptT
                      (runReachDef
                          ((interp :: Expr -> ReachDef ()) e)))
                              M.empty) emptyToySt

runFuncRD :: ToyModule -> String -> [RD.Set Value]
    -> (Either Interrupt (), ReachDefState)
runFuncRD mdl name vs =
    runState
        (runReaderT
            (runExceptT
                (runReachDef
                    ((interpFunc :: ToyModule -> String -> [RD.Set Value] -> ReachDef ()) mdl name vs)))
                        M.empty) emptyToySt
