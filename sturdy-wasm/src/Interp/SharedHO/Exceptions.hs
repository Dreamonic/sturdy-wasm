{-# LANGUAGE GADTs #-}

module Interp.SharedHO.Exceptions where

data TException where
    TypeMismatch :: (Show a, Show b) => a -> b -> TException

    -- |First argument is the operator
    InvalidOp :: (Show a, Show b, Show c) => a -> b -> c -> TException

    StackUnderflow ::TException

    -- |Takes the assigned type, variable name and existing type  
    InvalidAssignment :: (Show a, Show b, Show c) => a -> b -> c -> TException

    NotInScope :: (Show a) => a -> TException

    -- |Takes the target depth and max depth
    InvalidDepth :: (Show a, Show b) => a -> b -> TException


instance Show TException where
    show (TypeMismatch expected actual) =
        "Expected " ++ show expected ++ " but got " ++ show actual

    show (InvalidOp op x y) =
        "Cannot " ++ show op ++ " " ++ show x ++ " and " ++ show y

    show StackUnderflow = "Tried to pop value from empty stack"

    show (InvalidAssignment v var t) =
        "Cannot assign "
            ++ show v
            ++ " to "
            ++ show var
            ++ " of "
            ++ show t

    show (NotInScope var) = "Variable " ++ show var ++ " is not in scope"

    show (InvalidDepth target max) =
        "Can't break " ++ show target ++ " (max " ++ show max ++ ")"
