{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Interp.SharedHO.ToyInterpreter
where

import qualified Data.Map as M
import Prelude hiding (const, lookup)
import Control.Monad.State hiding (fix, join, state)
import Control.Monad.Reader hiding (fix, join)
import Control.Monad.Except hiding (fix, join)

import qualified Interp.SharedHO.RDSet as RD
import Interp.SharedHO.Joinable
import Interp.SharedHO.BoolVal

data Expr
    = Branch Int
    | Block Expr
    | Loop Expr
    | Seq [Expr]
    | Const Int
    | Add Expr Expr
    | Lt Expr Expr
    | If Expr Expr Expr
    | Assign String Expr
    | Var String

instance ToBool Int where
    toBool = (/=) 0

instance FromBool Int where
    fromBool b = if b then 1 else 0

-- Don't like the taste of fuel

-- fix :: (a -> a) -> a
-- fix f = f (fix f)
--
--
-- data Fuel a = Fuel { runFuel :: Int -> Maybe a }
--
-- returnF :: a -> Fuel a
-- returnF a = Fuel $ \_ -> Just a
--
-- bindF :: Fuel a -> (a -> Fuel b) -> Fuel b
-- bindF (Fuel f) g = Fuel $ \n -> case f n of
--     Just a  -> runFuel (g a) n
--     Nothing -> Nothing
--
-- fixF :: (a -> Fuel a) -> Fuel a
-- fixF f = Fuel $ \n -> if n <= 0
--     then Nothing
--     else case runFuel (fixF f) (n - 1) of
--         Just a  -> runFuel (f a) n
--         Nothing -> Nothing


class Monad m => Interp m a | m -> a where
    pushBlock :: m a -> m a -> m a
    popBlock :: Int -> m a
    const :: Int -> m a
    add :: a -> a -> m a
    lt :: a -> a -> m a
    if_ :: a -> m a -> m a -> m a
    assign :: String -> a -> m a
    lookup :: String -> m a

class Fix c where
    fix :: (c -> c) -> c

interp :: (Interp m a, Fix (m a)) => Expr -> m a
interp expr = case expr of
    Branch n -> popBlock n

    Block e -> pushBlock (const 0) (interp e)

    Loop e -> fix $ \lp -> pushBlock lp (interp e)

    Seq es -> foldl (>>) (const 0) (interp <$> es)

    Const n -> const n

    Add e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        add v1 v2

    Lt e1 e2 -> do
        v1 <- interp e1
        v2 <- interp e2
        lt v1 v2

    If e t f -> do
        c <- interp e
        if_ c (interp t) (interp f)

    Assign var e -> do
        v <- interp e
        assign var v

    Var var -> lookup var


newtype Concrete a = Concrete
    { runConcrete :: ExceptT (Either String Int) (State (M.Map String Int)) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String Int),
        MonadError (Either String Int))

instance Interp Concrete Int where
    pushBlock br adv = do
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg

    popBlock n = throwError $ Right n

    const = return

    add v1 v2 = return $ v1 + v2

    lt v1 v2 = return $ fromBool $ v1 < v2

    if_ c t f = if toBool c then t else f

    assign var v = do
        st <- get
        put $ M.insert var v st
        return v

    lookup var = do
        st <- get
        case M.lookup var st of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

instance Fix (Concrete Int) where
    fix f = f (fix f)

run :: Expr -> Either (Either String Int) Int
run e = fst $ runState
                  (runExceptT
                      (runConcrete
                          ((interp :: Expr -> Concrete Int) e))) M.empty


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

instance Fix (DepthChecker ()) where
    fix f = f (fix f)

check :: Expr -> Either String ()
check e = runExcept
            (runReaderT
                (runDepthChecker
                    ((interp :: Expr -> DepthChecker ()) e)) 0)

data SavedState a = SavedState { state :: a
                               , image :: a }

type ReachDefState = SavedState (M.Map String (RD.Set Int))

newtype ReachDef a = ReachDef
    { runReachDef :: ExceptT (Either String Int) (State ReachDefState) a }
    deriving (Functor, Applicative, Monad, MonadState ReachDefState,
        MonadError (Either String Int))

getSt :: ReachDef (M.Map String (RD.Set Int))
getSt = do
    st <- get
    return $ state st

getImg :: ReachDef (M.Map String (RD.Set Int))
getImg = do
    st <- get
    return $ image st

putSt :: M.Map String (RD.Set Int) -> ReachDef ()
putSt st = do
    img <- getImg
    put $ SavedState st img

putImg :: M.Map String (RD.Set Int) -> ReachDef ()
putImg img = do
    st <- getSt
    put $ SavedState st img

instance Joinable a => Joinable (ReachDef a) where
    join f g = do
        st <- getSt
        x <- f
        st1 <- getSt
        putSt st
        y <- g
        st2 <- getSt
        putSt $ st1 `join` st2
        return $ x `join` y

instance Interp ReachDef (RD.Set Int) where
    pushBlock br adv = do
        catchError adv $ \e -> case e of
            Right n  -> if n <= 0
                then br
                else throwError $ Right $ n - 1
            Left msg -> throwError $ Left msg

    popBlock n = throwError $ Right n

    const n = return $ RD.singleton n

    add v1 v2 = return $ RD.add v1 v2

    lt v1 v2 = return $ RD.lt v1 v2

    if_ c t f = RD.if_ c t f

    assign var v = do
        st <- getSt
        putSt $ M.insert var v st
        return v

    lookup var = do
        st <- getSt
        case M.lookup var st of
            Just v  -> return v
            Nothing -> throwError $ Left $ "Var " ++ var ++ " not in scope."

instance Fix (ReachDef (RD.Set Int)) where
    fix f = do
        st <- getSt
        img <- getImg
        if st == img
            then return RD.Top
            else do
                let st' = st `join` img
                putSt st'
                putImg st'
                f (fix f)

runRD :: Expr -> Either (Either String Int) (RD.Set Int)
runRD e = fst $ runState
                    (runExceptT
                        (runReachDef
                            ((interp :: Expr -> ReachDef (RD.Set Int)) e)))
                                (SavedState M.empty M.empty)
