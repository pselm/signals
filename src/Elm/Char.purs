module Elm.Char 
    ( module Virtual
    , KeyCode(), toCode, fromCode
    , isUpper, isLower, isDigit, isOctDigit, isHexDigit
    , toLocaleUpper, toLocaleLower
    ) where


-- For re-export

import Data.Char (toLower, toUpper) as Virtual


-- Internal

import Data.Char (toCharCode, fromCharCode)
import Elm.Basics


isBetween :: Char -> Char -> Char -> Bool
isBetween low high char =
    (code >= toCode low) && (code <= toCode high)
        where
            code =
                toCode char


{-| True for upper case letters. -}
isUpper :: Char -> Bool
isUpper = isBetween 'A' 'Z'


{-| True for lower case letters. -}
isLower :: Char -> Bool
isLower = isBetween 'a' 'z'


{-| True for ASCII digits `[0-9]`. -}
isDigit :: Char -> Bool
isDigit = isBetween '0' '9'


{-| True for ASCII octal digits `[0-7]`. -}
isOctDigit :: Char -> Bool
isOctDigit = isBetween '0' '7'


{-| True for ASCII hexadecimal digits `[0-9a-fA-F]`. -}
isHexDigit :: Char -> Bool
isHexDigit char =
    isDigit char || isBetween 'a' 'f' char || isBetween 'A' 'F' char


{-| Convert to upper case, according to any locale-specific case mappings. -}
foreign import toLocaleUpper :: Char -> Char


{-| Convert to lower case, according to any locale-specific case mappings. -}
foreign import toLocaleLower :: Char -> Char


{-| In this library, we use integers to represent the key codes coming from the
keyboard. You can use [`toCode`](#toCode) and [`fromCode`](#fromCode)
to convert between key codes and characters.
-}
type KeyCode = Int


{-| Convert to unicode. Used with the [`Keyboard`](Keyboard) library,
which expects the input to be uppercase.
-}
toCode :: Char -> KeyCode
toCode = toCharCode


{-| Convert from unicode. -}
fromCode :: KeyCode -> Char
fromCode = fromCharCode
