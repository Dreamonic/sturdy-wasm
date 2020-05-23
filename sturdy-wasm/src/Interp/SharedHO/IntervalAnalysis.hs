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
import Interp.SharedHO.Data.Interrupt

instance StdWideningSet (InfiniteNumber Value) where
    stdWSet = S.fromList [NegInfinity, -32, -16, -8, -4, -2, -1, 0, 1, 2, 4, 8,
                          16, 32, Infinity]

instance FromBool a => FromBool (InfiniteNumber a) where
    fromBool b = Number $ fromBool b

type IAnalysState = ToyState (Interval (InfiniteNumber Value))

newtype IAnalys a = IAnalys
    { runIAnalys :: ExceptT Interrupt (ReaderT (M.Map String (IAnalys ()))
        (State IAnalysState)) a }
    deriving (Functor, Applicative, Monad, MonadState IAnalysState,
        MonadReader (M.Map String (IAnalys ())), MonadError Interrupt)

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
            Branching n -> if n <= 0
                then br
                else throwError $ Branching $ n - 1
            Error msg   -> throwError $ Error msg
            Returning   -> throwError $ Returning
        st2 <- get
        v <- pop
        put $ set stack (v : (view stack st1)) st2

    popBlock n = throwError $ Branching n

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

    closure _ m = do
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

top :: Interval (InfiniteNumber Value)
top = Interval NegInfinity Infinity

fixIA :: IAnalysState -> (IAnalys () -> IAnalys ()) -> IAnalys ()
fixIA img f = do
    st <- get
    let st' = img `widening` st
    put st'
    if st' == img
        then push top
        else f $ fixIA st' f

instance Fix IAnalys where
    fix = fixIA emptyToySt

runIA :: Expr -> (Either Interrupt (), ToyState (Interval
    (InfiniteNumber Value)))
runIA e = runState
              (runReaderT
                  (runExceptT
                      (runIAnalys
                          ((interp :: Expr -> IAnalys ()) e)))
                              M.empty) emptyToySt

runFuncIA :: ToyModule -> String -> [Interval (InfiniteNumber Value)]
    -> (Either Interrupt (), IAnalysState)
runFuncIA mdl name vs =
    runState
        (runReaderT
            (runExceptT
                (runIAnalys
                    ((interpFunc :: ToyModule -> String
                        -> [Interval (InfiniteNumber Value)] -> IAnalys ())
                            mdl name vs))) M.empty) emptyToySt
