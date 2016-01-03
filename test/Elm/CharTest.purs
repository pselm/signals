module Test.Elm.CharTest (tests) where

import Test.Unit
import Test.Unit.Console

import Elm.Char 
import Prelude (bind, Eq)
import Elm.Basics ((<|), (==), (|>), (+), (-), flip)
import Elm.List as List
import Data.List (List(), (..))


assertEqual :: forall a. (Eq a) => String -> a -> a -> Test ( testOutput :: TestOutput )
assertEqual name expected actual =
    assert name <| expected == actual 


charRange :: Char -> Char -> List Char
charRange a b =
    List.map fromCode ((toCode a) .. (toCode b)) 


lower :: List Char
lower = charRange 'a' 'z'

upper :: List Char
upper = charRange 'A' 'Z'

dec :: List Char
dec = charRange '0' '9'

oct :: List Char
oct = List.take 8 dec

hexLower :: List Char
hexLower = List.take 6 lower

hexUpper :: List Char
hexUpper = List.take 6 upper

hex :: List Char
hex = List.append hexLower hexUpper |> List.append dec


lowerCodes :: List Int
lowerCodes = (97 .. (97 + List.length lower - 1))

upperCodes :: List Int
upperCodes = (65 .. (65 + List.length upper - 1))

decCodes :: List Int
decCodes = (48 .. (48 + List.length dec - 1))


oneOf :: forall a. (Eq a) => List a -> a -> Boolean
oneOf = flip List.member


tests :: Test ( testOutput :: TestOutput )
tests = do
    test "toCode" do
        assertEqual "a-z" (lowerCodes) (List.map toCode lower)
        assertEqual "A-Z" (upperCodes) (List.map toCode upper)
        assertEqual "0-9" (decCodes) (List.map toCode dec)
      
    test "fromCode" do
        assertEqual "a-z" (lower) (List.map fromCode lowerCodes)
        assertEqual "A-Z" (upper) (List.map fromCode upperCodes)
        assertEqual "0-9" (dec) (List.map fromCode decCodes)
      
    test "toLocaleLower" do
        assertEqual "a-z" (lower) (List.map toLocaleLower lower)
        assertEqual "A-Z" (lower) (List.map toLocaleLower upper)
        assertEqual "0-9" (dec) (List.map toLocaleLower dec) 

    test "toLocaleUpper" do
        assertEqual "a-z" (upper) (List.map toLocaleUpper lower)
        assertEqual "A-Z" (upper) (List.map toLocaleUpper upper)
        assertEqual "0-9" (dec) (List.map toLocaleUpper dec)
      
    test "toLower" do
        assertEqual "a-z" (lower) (List.map toLower lower)
        assertEqual "A-Z" (lower) (List.map toLower upper)
        assertEqual "0-9" (dec) (List.map toLower dec) 

    test "toUpper" do
        assertEqual "a-z" (upper) (List.map toUpper lower)
        assertEqual "A-Z" (upper) (List.map toUpper upper)
        assertEqual "0-9" (dec) (List.map toUpper dec) 

    test "isLower" do
        assertEqual "a-z" (true) (List.all isLower lower)
        assertEqual "A-Z" (false) (List.any isLower upper)
        assertEqual "0-9" (false) (List.any isLower dec)

    test "isUpper" do
        assertEqual "a-z" (false) (List.any isUpper lower)
        assertEqual "A-Z" (true) (List.all isUpper upper)
        assertEqual "0-9" (false) (List.any isUpper dec) 

    test "isDigit" do
        assertEqual "a-z" (false) (List.any isDigit lower)
        assertEqual "A-Z" (false) (List.any isDigit upper)
        assertEqual "0-9" (true) (List.all isDigit dec) 

    test "isHexDigit" do
        assertEqual "a-z" (List.map (oneOf hex) lower) (List.map isHexDigit lower)
        assertEqual "A-Z" (List.map (oneOf hex) upper) (List.map isHexDigit upper)
        assertEqual "0-9" (true) (List.all isHexDigit dec) 

    test "isOctDigit" do
      assertEqual "a-z" (false) (List.any isOctDigit lower)
      assertEqual "A-Z" (false) (List.any isOctDigit upper)
      assertEqual "0-9" (List.map (oneOf oct) dec) (List.map isOctDigit dec)
 
