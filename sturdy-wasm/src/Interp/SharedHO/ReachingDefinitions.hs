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

type ReachDefState = ToyState (RD.Set Value)

newtype ReachDef a = ReachDef
    { runReachDef :: ExceptT (Either String Int) (ReaderT ReachDefState
        (State ReachDefState)) a }
    deriving (Functor, Applicative, Monad, MonadState ReachDefState,
        MonadReader ReachDefState, MonadError (Either String Int))

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
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg
        st2 <- get
        v <- pop
        put $ set stack (v : (view stack st1)) st2

    popBlock n = throwError $ Right n

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
        let st' = st `join` img
        put st'
        if st' == img
            then return ()
            else local (\_ -> st') $ f (fix f)

runRD :: Expr -> (Either (Either String Int) (), ToyState (RD.Set Value))
runRD e = runState
              (runReaderT
                  (runExceptT
                      (runReachDef
                          ((interp :: Expr -> ReachDef ()) e)))
                              emptyToySt) emptyToySt
