{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Interp.SharedHO.ToyInterpreter
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)
import Control.Lens hiding (Const, assign)
import Control.Lens.TH

import qualified Interp.SharedHO.Data.RDSet as RD
import Interp.SharedHO.Data.Joinable
import Interp.SharedHO.Data.BoolVal

data Expr
    = Branch Int
    | Block Expr
    | Loop Expr
    | Seq [Expr]
    | Const Int
    | Add
    | Lt
    | If Expr Expr
    | Assign String
    | Var String
    | Nop

instance ToBool Int where
    toBool = (/=) 0

instance FromBool Int where
    fromBool b = if b then 1 else 0

class Monad m => Interp m v | m -> v where
    pushBlock :: m () -> m () -> m ()
    popBlock :: Int -> m ()
    const :: Int -> m v
    add :: v -> v -> m v
    lt :: v -> v -> m v
    if_ :: v -> m () -> m () -> m ()
    assign :: String -> v -> m ()
    lookup :: String -> m v
    push :: v -> m ()
    pop :: m v

class Fix c where
    fix :: (c -> c) -> c

interp :: (Interp m a, Fix (m ())) => Expr -> m ()
interp expr = case expr of
    Branch n -> popBlock n

    Block e -> pushBlock (return ()) (interp e)

    Loop e -> fix $ \br -> pushBlock br (interp e)

    Seq es -> sequence_ (interp <$> es)

    Const n -> do
        v <- const n
        push v

    Add -> do
        v1 <- pop
        v2 <- pop
        v3 <- add v2 v1
        push v3

    Lt -> do
        v1 <- pop
        v2 <- pop
        v3 <- lt v2 v1
        push v3

    If t f -> do
        c <- pop
        if_ c (interp t) (interp f)

    Assign var -> do
        v <- pop
        assign var v

    Var var -> do
        v <- lookup var
        push v

    Nop -> return ()


data ToyState v = ToyState { _variables :: M.Map String v
                           , _stack :: [v] } deriving (Show, Eq)

emptyToySt = ToyState M.empty []

makeLenses ''ToyState



-- Concrete interpreter

newtype Concrete a = Concrete
    { runConcrete :: ExceptT (Either String Int) (State (ToyState Int)) a }
    deriving (Functor, Applicative, Monad, MonadState (ToyState Int),
        MonadError (Either String Int))

instance Interp Concrete Int where
    pushBlock br adv = do
        st1 <- get
        put $ set stack [] st1
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg
        st2 <- get
        put $ over stack (++ view stack st1) st2

    popBlock n = throwError $ Right n

    const = return

    add v1 v2 = return $ v1 + v2

    lt v1 v2 = return $ fromBool $ v1 < v2

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

run :: Expr -> (Either (Either String Int) (), ToyState Int)
run e = runState
            (runExceptT
                (runConcrete
                    ((interp :: Expr -> Concrete ()) e))) emptyToySt



-- Depth checker

newtype DepthChecker a =
    DepthChecker { runDepthChecker :: (ReaderT Int (Except String) a) }
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

    lt _ _ = return ()

    add _ _ = return ()

    if_ _ t f = t >> f

    assign _ _ = return ()

    lookup _ = return ()

    push _ = return ()

    pop = return ()

instance Fix (DepthChecker ()) where
    fix f = f (fix f)

check :: Expr -> Either String ()
check e = runExcept
            (runReaderT
                (runDepthChecker
                    ((interp :: Expr -> DepthChecker ()) e)) 0)


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
    pushBlock br adv = do
        st1 <- get
        put $ set stack [] st1
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg
        st2 <- get
        put $ over stack (++ view stack st1) st2

    popBlock n = throwError $ Right n

    const n = return $ RD.singleton n

    add v1 v2 = return $ RD.add v1 v2

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
