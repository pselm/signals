module Test.Elm.RegexTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Regex
import Prelude (bind, Eq, (&&), not, show, map, (++))
import Elm.Basics ((<|), (==))
import Data.List (List(..), (:), zipWith)
import Data.Foldable (and)
import Elm.String (join)
import Data.Maybe (Maybe(..))
import Elm.Maybe (withDefault)


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual


matchEqual :: Match -> Match -> Boolean
matchEqual m1 m2 =
    m1.match == m2.match &&
    m1.submatches == m2.submatches &&
    m1.index == m2.index &&
    m1.number == m2.number


match :: String -> List (Maybe String) -> Int -> Int -> Match
match =
    { match: _
    , submatches: _
    , index: _
    , number: _
    }


tests :: forall e. TestUnit e
tests = test "Elm.Regex\n" do
    test "Regex" do
        -- contains
        let containsExp = regex("a")
        let containsTarget = "abcd"

        assert "contains twice" <|
            contains containsExp containsTarget
            &&
            contains containsExp containsTarget

        -- split
        let findComma = regex ","

        assertEqual "split All"
            ("a" : "b" : Nil)
            (split All findComma "a,b")

        assertEqual "split AtMost 1"
            ("a" : "b,c" : Nil)
            (split (AtMost 1) findComma "a,b,c")

        assertEqual "split All twice"
            ("a" : "b" : Nil)
            (split All findComma "a,b")

        assertEqual "split AtMost 1 twice"
            ("a" : "b,c" : Nil)
            (split (AtMost 1) findComma "a,b,c")


        -- find
        let findAny = regex "."
        let ab = "ab"
        
        assert "find All" <|
            and <|
                zipWith matchEqual
                    (match "a" Nil 0 1 : match "b" Nil 1 2 : Nil)
                    (find All findAny ab)


        assert "find All twice" <|
            ( and <|
                zipWith matchEqual
                    (match "a" Nil 0 1 : match "b" Nil 1 2 : Nil)
                    (find All findAny ab)
            ) &&
            ( and <|
                zipWith matchEqual
                    (match "a" Nil 0 1 : match "b" Nil 1 2 : Nil)
                    (find All findAny ab)
            )

        assert "find AtMost 1" <|
            and <|
                zipWith matchEqual
                    (match "a" Nil 0 1 : Nil)
                    (find (AtMost 1) findAny ab)

        assert "find All empty" <|
            and <|
                zipWith matchEqual
                    (match "" Nil 0 1 : Nil)
                    (find All (regex ".*") "")


        let findTwoCommas = find (AtMost 2) (regex ",")
        assert "findTwoCommas true" <| map _.index (findTwoCommas "a,b,c,d,e") == (1 : 3 : Nil)
        assert "findTwoCommas false" <| map _.index (findTwoCommas "a b c d e") == (Nil :: List Int)
        assert "findTwoCommas twice true" <| map _.index (findTwoCommas "a,b,c,d,e") == (1 : 3 : Nil)
        assert "findTwoCommas twice false" <| map _.index (findTwoCommas "a b c d e") == (Nil :: List Int)


        let places = find All (regex "([oi]n) a (big )?(\\w+)") "I am on a boat in a lake."

        assertEqual "find.match"
            (map _.match places)
            ("on a boat" : "in a lake" : Nil)

        assertEqual "find.submatches"
            (map _.submatches places)
            ( (Just "on" : Nothing : Just "boat" : Nil)
            : (Just "in" : Nothing : Just "lake" : Nil)
            : Nil
            )

        let bigPlaces = find All (regex "([oi]n) a (big )?(\\w+)") "I am on a big boat in a big lake."
        assertEqual "find.submatches with optionals"
            (map _.submatches bigPlaces)
            ( (Just "on" : Just "big " : Just "boat" : Nil)
            : (Just "in" : Just "big " : Just "lake" : Nil)
            : Nil
            )

        -- replace
        let vowels = regex "[aeiou]"
        let submatcher = regex "(large )?(br[^ ]*)" 
        let phrase = "The quick brown fox"

        assertEqual "replace AtMost 0"
            "The quick brown fox"
            (replace (AtMost 0) vowels (\_ -> "") phrase)

        assertEqual "replace AtMost 1"
            "Th quick brown fox"
            (replace (AtMost 1) vowels (\_ -> "") phrase)

        assertEqual "replace AtMost 2"
            "Th qick brown fox"
            (replace (AtMost 2) vowels (\_ -> "") phrase)

        assertEqual "replace All"
            "Th qck brwn fx"
            (replace All vowels (\_ -> "") phrase)

        assertEqual "replace match"
            "Thee quuiick broown foox"
            (replace All vowels (\m -> m.match ++ m.match) phrase)

        assertEqual "replace submatches"
            "The quick missing brown fox"
            (replace All submatcher (\m -> join "" (map (withDefault "missing ") m.submatches)) phrase)

        assertEqual "replace index"
            "Th2 q56ck br12wn f17x"
            (replace All vowels (\m -> show m.index) phrase)

        assertEqual "replace index"
            "Th1 q23ck br4wn f5x"
            (replace All vowels (\m -> show m.number) phrase)


        -- escape
        let unescaped = regex ("^a+")
        let escaped = regex (escape ("^a+"))
        
        assert "escaped true" <| contains escaped "^a+"
        assert "unescaped false" <| not <| contains unescaped "^a+"
        assert "escaped false" <| not <| contains escaped "aaabbb"
        assert "unescaped true" <| contains unescaped "aaabbb"

        assertEqual "show" "/match/g" (show <| regex "match")
        assertEqual "show insensitive" "/match/gi" (show <| caseInsensitive <| regex "match")

        
        -- caseInsensitive
        let sensitive = regex "a"
        let insensitive = caseInsensitive sensitive
        
        assert "caseInsensitve different case" <| contains insensitive "AAA"
        assert "sensitve different case" <| not <| contains sensitive "AAA"
        assert "caseInsensitve same case" <| contains insensitive "aaa"
        assert "sensitve same case" <| contains insensitive "aaa"
