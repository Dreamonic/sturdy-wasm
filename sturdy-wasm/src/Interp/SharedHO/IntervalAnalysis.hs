{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.IntervalAnalysis
where

import Data.Abstract.InfiniteNumbers
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Lens hiding (Const, assign)

import Interp.SharedHO.GenericInterpreter
import Interp.SharedHO.Data.Interval as Interval
import Interp.SharedHO.Data.Widening
import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.ToyState
import Interp.SharedHO.Data.Types
import Interp.SharedHO.Data.BoolVal

instance StdWideningSet (InfiniteNumber Value) where
    stdWSet = S.fromList [NegInfinity, -32, -16, -8, -4, -2, -1, 0, 1, 2, 4, 8,
                          16, 32, Infinity]

instance FromBool a => FromBool (InfiniteNumber a) where
    fromBool b = Number $ fromBool b

type IAnalysState = ToyState (Interval (InfiniteNumber Value))

newtype IAnalys a = IAnalys
    { runIAnalys :: ExceptT (Either String Int) (ReaderT IAnalysState
        (State IAnalysState)) a }
    deriving (Functor, Applicative, Monad, MonadState IAnalysState,
        MonadReader IAnalysState, MonadError (Either String Int))

instance Joinable a => Joinable (IAnalys a) where
    join f g = do
        st <- get
        x <- f
        st1 <- get
        put st
        y <- g
        st2 <- get
        put $ st1 `join` st2
        return $ x `join` y

instance Interp IAnalys (Interval (InfiniteNumber Value)) where
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

    const v = return $ Interval.degenerate $ Number v

    add v1 v2 = return $ Interval.add v1 v2

    eqz = return . Interval.eqz

    lt v1 v2 = return $ Interval.lt v1 v2

    if_ c t f = Interval.if_ c t f

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


instance Fix (IAnalys ()) where
    fix f = do
        st <- get
        img <- ask
        let st' = img `widening` st
        put st'
        if st' == img
            then return ()
            else local (\_ -> st') $ f (fix f)

runIA :: Expr -> (Either (Either String Int) (), ToyState (Interval
    (InfiniteNumber Value)))
runIA e = runState
              (runReaderT
                  (runExceptT
                      (runIAnalys
                          ((interp :: Expr -> IAnalys ()) e)))
                              emptyToySt) emptyToySt
