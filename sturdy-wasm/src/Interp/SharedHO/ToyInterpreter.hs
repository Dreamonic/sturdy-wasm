{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Interp.SharedHO.ToyInterpreter
    ( run
    , check
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (const, lookup)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

data Expr
    = Branch Int
    | Block Expr
    | Loop Expr
    | Seq Expr Expr
    | Const Int
    | Add Expr Expr
    | If Expr Expr Expr
    | Assign String Expr
    | Var String

interp :: Interp m a => Expr -> m a
interp expr = case expr of
    Branch n -> popBlock n

    Block e -> pushBlock (const 0) (interp e)

    Loop e -> pushBlock (interp e) (interp e)

    Seq e1 e2 -> interp e1 >> interp e2

    Const n -> const n

    Add e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        add v1 v2

    If e t f -> do
        c <- interp e
        if_ c (interp t) (interp f)

    Assign var e -> do
        v <- interp e
        assign var v

    Var var -> lookup var


class (Monad m) => Interp m a | m -> a where
    pushBlock :: m a -> m a -> m a
    popBlock :: Int -> m a
    const :: Int -> m a
    add :: a -> a -> m a
    if_ :: a -> m a -> m a -> m a
    assign :: String -> a -> m a
    lookup :: String -> m a

newtype Concrete a = Concrete
    { runConcrete :: ExceptT (Either String Int) (State (M.Map String Int)) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String Int),
        MonadError (Either String Int))

instance Interp Concrete Int where
    pushBlock br adv = do
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then pushBlock br br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg

    popBlock n = throwError $ Right n

    const = return

    add v1 v2 = return $ v1 + v2

    if_ c t f = if c == 0 then f else t

    assign var v = do
        st <- get
        put $ M.insert var v st
        return v

    lookup var = do
        st <- get
        case M.lookup var st of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

run :: Expr -> Either (Either String Int) Int
run e = fst $ runState
                  (runExceptT
                      (runConcrete
                          ((interp :: Expr -> Concrete Int) e))) M.empty


newtype DepthChecker a = DepthChecker { runDepthChecker :: (ReaderT Int (Except String) a) }
    deriving (Functor, Applicative, Monad, MonadReader Int, MonadError String)

instance Interp DepthChecker () where
    pushBlock _ adv = local (+1) adv

    popBlock n = do
        level <- ask
        if n >= level then
            throwError $ "Can't break " ++ show n
        else
            return ()

    const _ = return ()

    add _ _ = return ()

    if_ _ t f = t >> f

    assign _ _ = return ()

    lookup _ = return ()

check :: Expr -> Either String ()
check e = runExcept
            (runReaderT
                (runDepthChecker
                    ((interp :: Expr -> DepthChecker ()) e)) 0)

newtype ReachDef a = ReachDef
    { runReachDef :: ExceptT (Either String Int) (State (M.Map String (S.Set Int))) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String (S.Set Int)),
        MonadError (Either String Int))

instance Interp ReachDef (S.Set Int) where
    pushBlock br adv = do
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then pushBlock br br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg

    popBlock n = throwError $ Right n

    const n = return $ S.singleton n

    add v1 v2 = return $ S.map (\(x, y) -> x + y) (S.cartesianProduct v1 v2)

    if_ c t f = if
        | S.notMember 0 c -> f
        | S.size c == 1   -> t
        | otherwise       -> do
            st <- get
            v1 <- t
            st1 <- get
            put st
            v2 <- f
            st2 <- get
            put $ M.unionWith S.union st1 st2
            return $ v1 `S.union` v2

    assign var v = do
        st <- get
        put $ M.insert var v st
        return v

    lookup var = do
        st <- get
        case M.lookup var st of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."
