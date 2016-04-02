module Test.Elm.RandomTest (tests) where

import Test.Unit (TestUnit, test)
import Test.Unit.Assert (assert)

import Elm.Random
import Prelude (bind, (<), (-), (+), negate, map)
import Elm.Basics ((<|), (==), abs)
import Data.Int53 (Int53, fromInt)
import Data.List (toList)
import Data.Tuple (Tuple(..))


close :: Number -> Number -> Boolean
close a b =
    abs (a - b) < 0.00000001

infixl 9 close as ~=


seed1 :: Seed
seed1 = initialSeed 1283428


seed2 :: Seed
seed2 = initialSeed 8224729


seed3 :: Seed
seed3 = initialSeed 547283


genInt53 :: Generator Int53
genInt53 = int (fromInt 0) (fromInt 100)


genInt :: Generator Int
genInt = int 0 100


genFloat :: Generator Number
genFloat = float 0.0 100.0


doer :: Generator Int
doer = do
    y <- int 0 100
    int 0 y


tests :: forall e. TestUnit e
tests = test "Elm.Random\n" do
    test "Random.bool" do
        assert "seed1" <| false == (generate bool seed1).value
        assert "seed2" <| false == (generate bool seed2).value
        assert "seed3" <| true == (generate bool seed3).value

    test "Random.int53" do
        assert "seed1" <| fromInt 12 == (generate genInt53 seed1).value
        assert "seed2" <| fromInt 31 == (generate genInt53 seed2).value
        assert "seed3" <| fromInt 40 == (generate genInt53 seed3).value

    test "Random.int" do
        assert "seed1" <| 12 == (generate genInt seed1).value
        assert "seed2" <| 31 == (generate genInt seed2).value
        assert "seed3" <| 40 == (generate genInt seed3).value

    test "Random.float" do
        assert "seed1" <| 17.00685023 ~= (generate genFloat seed1).value
        assert "seed2" <| 63.96765709 ~= (generate genFloat seed2).value
        assert "seed3" <| 44.89097595 ~= (generate genFloat seed3).value

    test "Random.pair" do
        assert "seed1" <| Tuple false true == (generate (pair bool bool) seed1).value
        assert "seed2" <| Tuple false false == (generate (pair bool bool) seed2).value
        assert "seed3" <| Tuple true false == (generate (pair bool bool) seed3).value

    test "Random.list" do
        assert "seed1" <| (toList [12,46,21,91,94,27,68,99,78,6]) == (generate (list 10 genInt) seed1).value
        assert "seed2" <| (toList [31,54,62,55,16,15,34,59,88,41]) == (generate (list 10 genInt) seed2).value
        assert "seed3" <| (toList [40,52,34,2,95,2,70,49,53,67]) == (generate (list 10 genInt) seed3).value

    test "Random.array" do
        assert "seed1" <| [12,46,21,91,94,27,68,99,78,6] == (generate (list 10 genInt) seed1).value
        assert "seed2" <| [31,54,62,55,16,15,34,59,88,41] == (generate (list 10 genInt) seed2).value
        assert "seed3" <| [40,52,34,2,95,2,70,49,53,67] == (generate (list 10 genInt) seed3).value

    test "Ramdom.map" do
        assert "map"  <| 13 == (generate (map ((+) 1) genInt) seed1).value
        assert "map2" <| (-34) == (generate (map2 (-) genInt genInt) seed1).value
        assert "map3" <| 37 == (generate (map3 (\a b c -> a + b - c  ) genInt genInt genInt) seed1).value
        assert "map4" <| 78 == (generate (map4 (\a b c d -> a - b + c + d) genInt genInt genInt genInt) seed1).value
        assert "map5" <| 222 == (generate (map5 (\a b c d e -> a + b - c + d + e) genInt genInt genInt genInt genInt) seed1).value

    test "Random.andThen" do
        let genAndThen = genInt `andThen` int 0
        assert "seed1" <| 12 == (generate genAndThen seed1).value
        assert "seed2" <| 12 == (generate genAndThen seed2).value
        assert "seed3" <| 40 == (generate genAndThen seed3).value

    test "Random.do notation" do
        assert "seed1" <| 12 == (generate doer seed1).value
        assert "seed2" <| 12 == (generate doer seed2).value
        assert "seed3" <| 40 == (generate doer seed3).value
