module Tokens(
    Token(..)
    , idchar
    , idcharRegex
) where

-- |    All possible tokens, based upon the syntax format of Web Assembly.
data Token
    = Keyword String        -- ^ A keyword indicating a function or system call.
    | UnsignedN Integer     -- ^ An unsigned integer.
    | SignedN Integer       -- ^ A signed integer.
    | FloatN Double         -- ^ A float.
    | Str String            -- ^ A string.
    | ID String             -- ^ An ID, indicating a certain value.
    | LP                    -- ^ A left parenthesis.
    | RP                    -- ^ A right parenthesis.
    | Reserved String       -- ^ Any value not defined.
    deriving (Show, Eq)


-- |    This is a list of each possible idchar.
idchar :: String
idchar = ['0'..'9'] 
    ++ ['a'..'z'] 
    ++ ['A'..'Z'] 
    ++ ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '/']
    ++ [':', '<', '=', '>', '?', '@', '\\', '^', '_', '`', '|', '~']

-- |    Represents a string, containing every possible idchar,
--      with certain characters escaped.
idcharRegex :: String
idcharRegex = "[0-9a-zA-Z!<=>\\?#\\$%&\'\\*\\+-\\./:<=>\\?@\\\\\\^_`\\|~]"

-- |    Unused, but utility function to check whether a char is an idchar.
isIDChar :: Char -> Bool
isIDChar c = c `elem` idchar

-- |    Unused, but represents seperator symbols.
seperatorSymbols :: String
seperatorSymbols = "\\s"