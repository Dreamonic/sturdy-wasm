module Exception(
    ExecutorException(..)
    , executorCatch
) where

import Parser
import qualified Control.Exception as E

-- |All exceptions that can be thrown by the executor.
data ExecutorException
    = TypeError String          -- ^For failed type matching.
    | LookupError String        -- ^For variable lookup errors.
    | WasmArithError String     -- ^For arithmetic errors that would have been
                                --  runtime errors in WASM (like div by zero).
    | NotImplemented Instr      -- ^For unimplemented instruction.
    deriving Show

instance E.Exception ExecutorException

-- |Catches ExecutorExceptions by using the given handler function that are
--  possible thrown by the given expression.
executorCatch :: (ExecutorException  -> IO a) -> a -> IO a
executorCatch handler x = E.catch (E.evaluate x) handler