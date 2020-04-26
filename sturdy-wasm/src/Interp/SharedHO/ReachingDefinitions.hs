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
import Control.Lens.TH

import qualified Interp.SharedHO.RDSet as RD
import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.ConcreteInterpreter (ToyState, stack, variables, emptyToySt)
import Interp.SharedHO.Joinable
import Interp.SharedHO.Types

type ReachDefState = ToyState (RD.Set Int)

newtype ReachDef a = ReachDef
    { runReachDef :: ExceptT (Either String Int) (ReaderT ReachDefState
        (State ReachDefState)) a }
    deriving (Functor, Applicative, Monad, MonadState ReachDefState,
        MonadReader ReachDefState, MonadError (Either String Int))

instance Joinable a => Joinable (ToyState a) where
    join st1 st2 = over stack (join $ view stack st2) $
                   over variables (join $ view variables st2) st1

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

instance Interp ReachDef (RD.Set Int) where
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
        put $ set stack (v : (view stack st1)) st2

    popBlock n = throwError $ Right n

    const n = return $ RD.singleton $ fromIntegral (getVal n)

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
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

    push v = modify $ over stack (v:)

    pop = do
        st <- get
        case view stack st of
            v:_ -> do
                modify $ over stack tail
                return v
            []  -> throwError $ Left $ "Tried to pop value from empty stack."


instance Fix (ReachDef ()) where
    fix f = do
        st <- get
        img <- ask
        if st == img
            then return ()
            else do
                let st' = st `join` img
                put st'
                local (\_ -> st') $ f (fix f)

runRD :: Expr -> (Either (Either String Int) (), ToyState (RD.Set Int))
runRD e = runState
              (runReaderT
                  (runExceptT
                      (runReachDef
                          ((interp :: Expr -> ReachDef ()) e)))
                              emptyToySt) emptyToySt