{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interp.SharedHO.ToyInterpreter
    ( run
    , check
    ) where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.Identity
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

    If e t f -> if_ (interp e) (interp t) (interp f)

    Assign var e -> assign var (interp e)

    Var var -> lookup var


class (Monad m) => Interp m a | m -> a where
    pushBlock :: m a -> m a -> m a
    popBlock :: Int -> m a
    const :: Int -> m a
    add :: a -> a -> m a
    if_ :: m a -> m a -> m a -> m a
    assign :: String -> m a -> m a
    lookup :: String -> m a


newtype Concrete a = Concrete
    { runConcrete :: StateT (M.Map String Int) (ExceptT (Either String Int)
        Identity) a }
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

    if_ c t f = do
        c' <- c
        if c' == 0 then f else t

    assign var f = do
        v <- f
        st <- get
        put $ M.insert var v st
        f

    lookup var = do
        st <- get
        case M.lookup var st of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

run :: Expr -> Either (Either String Int) Int
run e = fst <$> runExcept
                    (runStateT
                        (runConcrete
                            ((interp :: Expr -> Concrete Int) e)) M.empty)


newtype DepthChecker a = DepthChecker { runDepthChecker :: (ReaderT Int (ExceptT String Identity) a) }
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

    if_ c t f = c >> t >> f

    assign _ _ = return ()

    lookup _ = return ()

check :: Expr -> Either String ()
check e = runExcept
            (runReaderT
                (runDepthChecker
                    ((interp :: Expr -> DepthChecker ()) e)) 0)
