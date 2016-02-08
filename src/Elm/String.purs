
-- | A built-in representation for efficient string manipulation. String literals
-- | are enclosed in `"double quotes"`. Strings are *not* lists of characters.
-- |
-- | This is implemented in terms of Purescript's `String` type, so you can also
-- | use functions from `Data.String`.

module Elm.String 
    ( module Virtual
    , isEmpty, cons
    , startsWith, endsWith
    , reverse, repeat, concat
    , split, join, slice
    , map, filter, foldl, foldr
    , left, right, dropLeft, dropRight
    , pad, padLeft, padRight
    , trimLeft, trimRight
    , words, lines
    , any, all
    , indexes, indices
    , toInt, toFloat
    , toList, fromList
    ) where


-- For re-export

import Data.String 
    ( fromChar, uncons, length, trim
    , toUpper, toLower, contains
    ) as Virtual

import Prelude (append) as Virtual


-- Internal

import Data.String (null, fromCharArray, toCharArray, fromChar, length, take, drop)
import Elm.Result (Result(..))
import Elm.Basics (Bool, Float, (<|))
import Elm.Char (isDigit)
import Data.List (List())
import Prelude ((++), (<<<), (>>>), (-), (/))
import Data.Foldable (mconcat)


-- | Determine if a string is empty.
-- | 
-- |     isEmpty "" == True
-- |     isEmpty "the world" == False
-- |
-- | Equivalent to Purescript's `null`.
isEmpty :: String -> Bool
isEmpty = null


-- | Add a character to the beginning of a string.
cons :: Char -> String -> String
cons c = (++) (fromChar c)


-- | Concatenate many strings into one.
-- | 
-- |     concat ["never","the","less"] == "nevertheless"
-- |
-- | Equivalent to Purescript's `mconcat`
concat :: List String -> String
concat = mconcat


-- | Transform every character in a string
-- | 
-- |     map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
map :: (Char -> Char) -> String -> String
map func string =
    fromCharArray <|
        Prelude.map func (toCharArray string)
        

-- | Keep only the characters that satisfy the predicate.
-- | 
-- |     filter isDigit "R2-D2" == "22"
filter :: (Char -> Bool) -> String -> String
filter func string =
    fromCharArray <|
        Data.Array.filter func (toCharArray string)


-- | Reverse a string.
-- | 
-- |     reverse "stressed" == "desserts"
reverse :: String -> String
reverse string =
    -- It feels as though there should be a better way to do this
    fromCharArray <|
        Data.Array.reverse (toCharArray string)


-- | Reduce a string from the left.
-- | 
-- |     foldl cons "" "time" == "emit"
foreign import foldl :: forall b. (Char -> b -> b) -> b -> String -> b


-- | Reduce a string from the right.
-- | 
-- |     foldr cons "" "time" == "time"
foreign import foldr :: forall b. (Char -> b -> b) -> b -> String -> b


-- | Split a string using a given separator.
-- | 
-- |     split "," "cat,dog,cow"        == ["cat","dog","cow"]
-- |     split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]
-- | 
-- | Use [`Regex.split`](Regex#split) if you need something more flexible.
split :: String -> String -> List String
split sep s =
    Data.List.toList <|
        Data.String.split sep s 


-- | Put many strings together with a given separator.
-- | 
-- |     join "a" ["H","w","ii","n"]        == "Hawaiian"
-- |     join " " ["cat","dog","cow"]       == "cat dog cow"
-- |     join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
join :: String -> List String -> String
join sep list = Data.String.joinWith sep (Data.List.fromList list)


-- | Repeat a string *n* times.
-- | 
-- |     repeat 3 "ha" == "hahaha"
foreign import repeat :: Int -> String -> String


-- | Take a substring given a start and end index. Negative indexes
-- | are taken starting from the *end* of the list.
-- | 
-- |     slice  7  9 "snakes on a plane!" == "on"
-- |     slice  0  6 "snakes on a plane!" == "snakes"
-- |     slice  0 -7 "snakes on a plane!" == "snakes on a"
-- |     slice -6 -1 "snakes on a plane!" == "plane"
foreign import slice :: Int -> Int -> String -> String


-- | Take *n* characters from the left side of a string.
-- |
-- | Equivalent to Purescript's `left`.
left :: Int -> String -> String
left = take


-- | Take *n* characters from the right side of a string.
foreign import right :: Int -> String -> String


-- | Drop *n* characters from the left side of a string.
-- |
-- | Equivalent to Purescript's `drop`.
dropLeft :: Int -> String -> String
dropLeft = drop


-- | Drop *n* characters from the right side of a string.
foreign import dropRight :: Int -> String -> String


-- | Pad a string on both sides until it has a given length.
-- | 
-- |     pad 5 ' ' "1"   == "  1  "
-- |     pad 5 ' ' "11"  == "  11 "
-- |     pad 5 ' ' "121" == " 121 "
pad :: Int -> Char -> String -> String
pad desiredLength padding string =
    let
        padder =
            fromChar padding

        addSpaces =
            desiredLength - (length string)

        addToRight =
            addSpaces / 2

        addToLeft =
            addSpaces - addToRight

    in
        (repeat addToLeft padder) ++
        string ++
        (repeat addToRight padder)


-- | Pad a string on the left until it has a given length.
-- | 
-- |     padLeft 5 '.' "1"   == "....1"
-- |     padLeft 5 '.' "11"  == "...11"
-- |     padLeft 5 '.' "121" == "..121"
padLeft :: Int -> Char -> String -> String
padLeft desiredLength padding string =
    let
        padder =
            fromChar padding

        addSpaces =
            desiredLength - (length string)

    in
        (repeat addSpaces padder) ++ string


-- | Pad a string on the right until it has a given length.
-- | 
-- |     padRight 5 '.' "1"   == "1...."
-- |     padRight 5 '.' "11"  == "11..."
-- |     padRight 5 '.' "121" == "121.."
padRight :: Int -> Char -> String -> String
padRight desiredLength padding string =
    let
        padder =
            fromChar padding

        addSpaces =
            desiredLength - (length string)

    in
        string ++ (repeat addSpaces padder)


-- | Get rid of whitespace on the left of a string.
-- | 
-- |     trimLeft "  hats  \n" == "hats  \n"
foreign import trimLeft :: String -> String


-- | Get rid of whitespace on the right of a string.
-- | 
-- |     trimRight "  hats  \n" == "  hats"
foreign import trimRight :: String -> String


-- | Break a string into words, splitting on chunks of whitespace.
-- |
-- |     words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
words :: String -> List String
words = Data.List.toList <<< _words


foreign import _words :: String -> Array String


-- | Break a string into lines, splitting on newlines.
-- | 
-- |     lines "How are you?\nGood?" == ["How are you?", "Good?"]
lines :: String -> List String
lines = Data.List.toList <<< _lines


foreign import _lines :: String -> Array String


-- | Determine whether *any* characters satisfy a predicate.
-- | 
-- |     any isDigit "90210" == True
-- |     any isDigit "R2-D2" == True
-- |     any isDigit "heart" == False
foreign import any :: (Char -> Bool) -> String -> Bool


-- | Determine whether *all* characters satisfy a predicate.
-- | 
-- |     all isDigit "90210" == True
-- |     all isDigit "R2-D2" == False
-- |     all isDigit "heart" == False
foreign import all :: (Char -> Bool) -> String -> Bool


-- | See if the second string starts with the first one.
-- | 
-- |     startsWith "the" "theory" == True
-- |     startsWith "ory" "theory" == False
foreign import startsWith :: String -> String -> Bool


-- | See if the second string ends with the first one.
-- | 
-- |     endsWith "the" "theory" == False
-- |     endsWith "ory" "theory" == True
foreign import endsWith :: String -> String -> Bool


-- | Get all of the indexes for a substring in another string.
-- | 
-- |     indexes "i" "Mississippi"   == [1,4,7,10]
-- |     indexes "ss" "Mississippi"  == [2,5]
-- |     indexes "needle" "haystack" == []
indexes :: String -> String -> List Int
indexes little big =
    Data.List.toList <|
        _indexes little big 


foreign import _indexes :: String -> String -> Array Int


-- | Alias for `indexes`.
indices :: String -> String -> List Int
indices = indexes


type ConversionHelper a =
    { ok :: a -> Result String a
    , err :: String -> Result String a
    , isDigit :: Char -> Boolean
    }

foreign import _toInt :: ConversionHelper Int -> String -> Result String Int
foreign import _toFloat :: ConversionHelper Float -> String -> Result String Float

conversionHelper :: forall a. ConversionHelper a
conversionHelper =
    { ok: Ok
    , err: Err
    , isDigit: isDigit
    }


-- | Try to convert a string into an int, failing on improperly formatted strings.
-- | 
-- |     toInt "123" == Ok 123
-- |     toInt "-42" == Ok -42
-- |     toInt "3.1" == Err "could not convert string '3.1' to an Int"
-- |     toInt "31a" == Err "could not convert string '31a' to an Int"
toInt :: String -> Result String Int
toInt = _toInt conversionHelper 


-- | Try to convert a string into a float, failing on improperly formatted strings.
-- | 
-- |     toFloat "123" == Ok 123.0
-- |     toFloat "-42" == Ok -42.0
-- |     toFloat "3.1" == Ok 3.1
-- |     toFloat "31a" == Err "could not convert string '31a' to a Float"
toFloat :: String -> Result String Float
toFloat = _toFloat conversionHelper


-- | Convert a string to a list of characters.
-- | 
-- |     toList "abc" == ['a','b','c']
toList :: String -> List Char
toList = Data.List.toList <<< toCharArray


-- | Convert a list of characters into a String. Can be useful if you
-- | want to create a string primarily by consing, perhaps for decoding
-- | something.
-- | 
-- |     fromList ['a','b','c'] == "abc"
fromList :: List Char -> String
fromList = Data.List.fromList >>> fromCharArray

