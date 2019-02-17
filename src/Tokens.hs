module Tokens(
    Token(..)
    , idchar
    , idcharRegex
) where

-- | All possible tokens, based upon the syntax format of Web Assembly.
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

idchar :: String
idchar = ['0'..'9'] 
    ++ ['a'..'z'] 
    ++ ['A'..'Z'] 
    ++ ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '/']
    ++ [':', '<', '=', '>', '?', '@', '\\', '^', '_', '`', '|', '~']

idcharRegex :: String
idcharRegex = "[0-9a-zA-Z!<=>\\?#\\$%&\'\\*\\+-\\./:<=>\\?@\\\\\\^_`\\|~]"

isIDChar :: Char -> Bool
isIDChar c = c `elem` idchar

seperatorSymbols :: String
seperatorSymbols = "\\s"