module Test.Elm.StringTest (tests) where

import Test.Unit (TestUnit, Assertion, test)
import Test.Unit.Assert (assert)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.QuickCheck as QC

import Elm.String
import Control.Monad.Eff.Random (RANDOM())
import Prelude (bind, class Eq, not, (&&), ($), (*), (>), (+))
import Elm.Basics ((<|), (==), (/=), negate)
import Elm.Maybe (Maybe(..))
import Elm.Result (Result(..))
import Data.List (toList) as List


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual 


assertResult :: forall a e. (Eq a) => String -> Result String a -> Result String a -> Assertion e
assertResult name expected actual =
    assert name <| expected == actual 


tests :: forall e. TestUnit (random :: RANDOM | e)
tests = test "Elm.String\n" do
    test "Simple Stuff" do
        assert "is empty" (isEmpty "")
        assert "is not empty" (not (isEmpty ("the world")))
        assertEqual "length" 11 (length "innumerable")
        assert "endsWith" (endsWith "ship" "spaceship")
        assertEqual "reverse" "desserts" (reverse "stressed")
        assertEqual "repeat" "hahaha" (repeat 3 "ha")
        assertEqual "fromChar" "c" (fromChar 'c')
        assertEqual "trim" "hats" (trim "   hats  \n")

    test "Combining " do
        assertEqual "cons" "cab" (cons 'c' "ab")
        
        let result = uncons "abc"
        
        assert "uncons non-empty" <|
            case result of
                Just val -> val.head == 'a' && val.tail == "bc"
                Nothing -> false

        assert "uncons empty" <|
            case uncons "" of
                 Just _ -> false
                 Nothing -> true
        
        assertEqual "append 1" "butterfly" (append "butter" "fly")
        assertEqual "append 2" "butter" (append "butter" "")
        assertEqual "append 3" "butter" (append "" "butter")
        
        assertEqual "concat" "nevertheless" (concat <| List.toList ["never","the","less"])
        assertEqual "concat (array)" "nevertheless" (concat <| ["never","the","less"])
        
        assertEqual "split commas" (List.toList ["cat","dog","cow"]) (split "," "cat,dog,cow")
        assertEqual "split slashes" (List.toList ["home","steve","Desktop", ""]) (split "/" "home/steve/Desktop/")
        
        assertEqual "join spaces"  "cat dog cow" (join " " <| List.toList ["cat","dog","cow"])
        assertEqual "join slashes" "home/steve/Desktop" (join "/" <| List.toList ["home","steve","Desktop"])
        
        assertEqual "slice 1" "c" (slice 2 3 "abcd")
        assertEqual "slice 2" "abc" (slice 0 3 "abcd")
        assertEqual "slice 3" "abc" (slice 0 (-1) "abcd")
        assertEqual "slice 4" "cd" (slice (-2) 4 "abcd")

    test "Mapping" do
        assertEqual "map" "a.b.c" (map (\c -> if c == '/' then '.' else c) "a/b/c")
        assertEqual "filter" "abc" (filter (\c -> c /= '.') "a.b.c")
        assertEqual "foldl" "emit" (foldl cons "" "time")
        assertEqual "foldr" "time" (foldr cons "" "time")
        
    test "left and right" do     
        assertEqual "left" "go" (left 2 "go left")
        assertEqual "right" "left" (right 4 "go left")
        assertEqual "dropLeft" "left" (dropLeft 3 "go left")
        assertEqual "dropRight" "go" (dropRight 5 "go left")

    test "padding, trimming, splitting" do
        assert "pad 1" <| pad 5 ' ' "1"   == "  1  "
        assert "pad 2" <| pad 5 ' ' "11"  == "  11 "
        assert "pad 3" <| pad 5 ' ' "121" == " 121 "
        
        assert "padLeft 1" <| padLeft 5 '.' "1"   == "....1"
        assert "padLeft 2" <| padLeft 5 '.' "11"  == "...11"
        assert "padLeft 3" <| padLeft 5 '.' "121" == "..121"
        
        assert "padRight 1" <| padRight 5 '.' "1"   == "1...."
        assert "padRight 2" <| padRight 5 '.' "11"  == "11..."
        assert "padRight 3" <| padRight 5 '.' "121" == "121.."
        
        assert "trimLeft" <| trimLeft "  hats  \n" == "hats  \n"
        assert "trimRight" <| trimRight "  hats  \n" == "  hats"
        
        assert "words" <| words "How are \t you? \n Good?" == List.toList ["How","are","you?","Good?"]
        assert "lines" <| lines "How are you?\nGood?" == List.toList ["How are you?", "Good?"]

    test "case" do
        assertEqual "toUpper" "UPPER" <| toUpper "Upper" 
        assertEqual "toLower" "upper" <| toLower "Upper"

    test "logic" do
        assert "any true" <| any (\c -> c == '.') "bob." 
        assert "any false" <| not <| any (\c -> c == '.') "bob"
        
        assert "all true" <| all (\c -> c == '.') "..."
        assert "all false" <| not <| all (\c -> c == '.') "bob."

        assert "contains 1" <| contains "the" "theory"
        assert "contains 2" <| not <| contains "hat" "theory"
        assert "contains 3" <| not <| contains "THE" "theory"
    
        assert "startsWith true" <| startsWith "the" "theory"
        assert "startsWith false" <| not <| startsWith "ory" "theory"
    
        assert "endsWith false" <| not <| endsWith "the" "theory"
        assert "endsWith true" <| endsWith "ory" "theory"

        assert "indexes 1" <| indexes "i" "Mississippi"   == List.toList [1,4,7,10]
        assert "indexes 2" <| indexes "ss" "Mississippi"  == List.toList [2,5]
        assert "indexes 3" <| indexes "needle" "haystack" == List.toList []

        assert "indices 1" <| indices "i" "Mississippi"   == List.toList [1,4,7,10]
        assert "indices 2" <| indices "ss" "Mississippi"  == List.toList [2,5]
        assert "indices 3" <| indices "needle" "haystack" == List.toList []
    
    test "conversions" do
        assertResult "toInt 1" (toInt "123") (Ok 123)
        assertResult "toInt 2" (toInt "-42") (Ok (-42))
        assertResult "toInt 3" (toInt "3.1") (Err "could not convert string '3.1' to an Int")
        assertResult "toInt 4" (toInt "31a") (Err "could not convert string '31a' to an Int")
    
        assertResult "toFloat 1" (toFloat "123") (Ok 123.0)
        assertResult "toFloat 2" (toFloat "-42") (Ok (-42.0))
        assertResult "toFloat 3" (toFloat "3.1") (Ok 3.1)
        assertResult "toFloat 4" (toFloat "31a") (Err "could not convert string '31a' to a Float")
    
        assert "toList" <| toList "abc" == List.toList ['a','b','c']
        assert "fromList" <| fromList (List.toList ['a','b','c']) == "abc"

    quick


repeatLength :: Int -> String -> QC.Result
repeatLength a b =
    length (repeat a b) ===
        if a > 0
           then a * length b
           else 0


isEmptyLength :: String -> QC.Result
isEmptyLength a =
    (length a == 0) === isEmpty a


checkConsLength :: Char -> String -> QC.Result
checkConsLength a b =
    length (cons a b) === (length b) + 1


quick :: forall e. TestUnit (random :: RANDOM | e)
quick = do
    test "repeatLength" $ quickCheck repeatLength
    test "isEmpty" $ quickCheck isEmptyLength
