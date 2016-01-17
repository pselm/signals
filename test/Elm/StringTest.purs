module Test.Elm.StringTest (tests) where

import Test.Unit
import Test.Unit.Assert

import qualified Elm.String as String
import Prelude (bind, Eq, not, (&&))
import Elm.Basics ((<|), (==), (/=), negate)
import Elm.Maybe (Maybe(..))
import Elm.Result (Result(..))
import Data.List (toList)


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual 


assertResult :: forall a e. (Eq a) => String -> Result String a -> Result String a -> Assertion e 
assertResult name expected actual =
    assert name <| expected == actual 


tests :: forall e. TestUnit e
tests = do
    test "Simple Stuff" do
        assert "is empty" (String.isEmpty "")
        assert "is not empty" (not (String.isEmpty ("the world")))
        assertEqual "length" 11 (String.length "innumerable")
        assert "endsWith" (String.endsWith "ship" "spaceship")
        assertEqual "reverse" "desserts" (String.reverse "stressed")
        assertEqual "repeat" "hahaha" (String.repeat 3 "ha")
        assertEqual "fromChar" "c" (String.fromChar 'c')
        assertEqual "trim" "hats" (String.trim "   hats  \n")

    test "Combining Strings" do
        assertEqual "cons" "cab" (String.cons 'c' "ab")
        
        let result = String.uncons "abc"
        
        assert "uncons non-empty" <|
            case result of
                Just val -> val.head == 'a' && val.tail == "bc"
                Nothing -> false

        assert "uncons empty" <|
            case String.uncons "" of
                 Just _ -> false
                 Nothing -> true
        
        assertEqual "append 1" "butterfly" (String.append "butter" "fly")
        assertEqual "append 2" "butter" (String.append "butter" "")
        assertEqual "append 3" "butter" (String.append "" "butter")
        
        assertEqual "concat" "nevertheless" (String.concat <| toList ["never","the","less"])
        
        assertEqual "split commas" (toList ["cat","dog","cow"]) (String.split "," "cat,dog,cow")
        assertEqual "split slashes" (toList ["home","steve","Desktop", ""]) (String.split "/" "home/steve/Desktop/")
        
        assertEqual "join spaces"  "cat dog cow" (String.join " " <| toList ["cat","dog","cow"])
        assertEqual "join slashes" "home/steve/Desktop" (String.join "/" <| toList ["home","steve","Desktop"])
        
        assertEqual "slice 1" "c" (String.slice 2 3 "abcd")
        assertEqual "slice 2" "abc" (String.slice 0 3 "abcd")
        assertEqual "slice 3" "abc" (String.slice 0 (-1) "abcd")
        assertEqual "slice 4" "cd" (String.slice (-2) 4 "abcd")

    test "Mapping" do
        assertEqual "map" "a.b.c" (String.map (\c -> if c == '/' then '.' else c) "a/b/c")
        assertEqual "filter" "abc" (String.filter (\c -> c /= '.') "a.b.c")
        assertEqual "foldl" "emit" (String.foldl String.cons "" "time")
        assertEqual "foldr" "time" (String.foldr String.cons "" "time")
        
    test "left and right" do     
        assertEqual "left" "go" (String.left 2 "go left")
        assertEqual "right" "left" (String.right 4 "go left")
        assertEqual "dropLeft" "left" (String.dropLeft 3 "go left")
        assertEqual "dropRight" "go" (String.dropRight 5 "go left")

    test "padding, trimming, splitting" do
        assert "pad 1" <| String.pad 5 ' ' "1"   == "  1  "
        assert "pad 2" <| String.pad 5 ' ' "11"  == "  11 "
        assert "pad 3" <| String.pad 5 ' ' "121" == " 121 "
        
        assert "padLeft 1" <| String.padLeft 5 '.' "1"   == "....1"
        assert "padLeft 2" <| String.padLeft 5 '.' "11"  == "...11"
        assert "padLeft 3" <| String.padLeft 5 '.' "121" == "..121"
        
        assert "padRight 1" <| String.padRight 5 '.' "1"   == "1...."
        assert "padRight 2" <| String.padRight 5 '.' "11"  == "11..."
        assert "padRight 3" <| String.padRight 5 '.' "121" == "121.."
        
        assert "trimLeft" <| String.trimLeft "  hats  \n" == "hats  \n"
        assert "trimRight" <| String.trimRight "  hats  \n" == "  hats"
        
        assert "words" <| String.words "How are \t you? \n Good?" == toList ["How","are","you?","Good?"]
        assert "lines" <| String.lines "How are you?\nGood?" == toList ["How are you?", "Good?"]

    test "case" do
        assertEqual "toUpper" "UPPER" <| String.toUpper "Upper" 
        assertEqual "toLower" "upper" <| String.toLower "Upper"

    test "logic" do
        assert "any true" <| String.any (\c -> c == '.') "bob." 
        assert "any false" <| not <| String.any (\c -> c == '.') "bob"
        
        assert "all true" <| String.all (\c -> c == '.') "..."
        assert "all false" <| not <| String.all (\c -> c == '.') "bob."

        assert "contains 1" <| String.contains "the" "theory"
        assert "contains 2" <| not <| String.contains "hat" "theory"
        assert "contains 3" <| not <| String.contains "THE" "theory"
    
        assert "startsWith true" <| String.startsWith "the" "theory"
        assert "startsWith false" <| not <| String.startsWith "ory" "theory"
    
        assert "endsWith false" <| not <| String.endsWith "the" "theory"
        assert "endsWith true" <| String.endsWith "ory" "theory"

        assert "indexes 1" <| String.indexes "i" "Mississippi"   == toList [1,4,7,10]
        assert "indexes 2" <| String.indexes "ss" "Mississippi"  == toList [2,5]
        assert "indexes 3" <| String.indexes "needle" "haystack" == toList []

        assert "indices 1" <| String.indices "i" "Mississippi"   == toList [1,4,7,10]
        assert "indices 2" <| String.indices "ss" "Mississippi"  == toList [2,5]
        assert "indices 3" <| String.indices "needle" "haystack" == toList []
    
    test "conversions" do
        assertResult "toInt 1" (String.toInt "123") (Ok 123)
        assertResult "toInt 2" (String.toInt "-42") (Ok (-42))
        assertResult "toInt 3" (String.toInt "3.1") (Err "could not convert string '3.1' to an Int")
        assertResult "toInt 4" (String.toInt "31a") (Err "could not convert string '31a' to an Int")
    
        assertResult "toFloat 1" (String.toFloat "123") (Ok 123.0)
        assertResult "toFloat 2" (String.toFloat "-42") (Ok (-42.0))
        assertResult "toFloat 3" (String.toFloat "3.1") (Ok 3.1)
        assertResult "toFloat 4" (String.toFloat "31a") (Err "could not convert string '31a' to a Float")
    
        assert "toList" <| String.toList "abc" == toList ['a','b','c']
        assert "fromList" <| String.fromList (toList ['a','b','c']) == "abc"
