module Test.Elm.Int53Test (tests) where

import Test.Unit
import Test.Unit.Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck ((===), Result())

import Elm.Int53
import Control.Monad.Eff.Random
import Prelude (bind, Eq, negate, not, top, bottom, ($))
import Elm.Basics ((<|), (==))
import Data.Maybe
import qualified Data.Int as Int


nothing :: Maybe Int53
nothing = Nothing


tests :: forall e. TestUnit (random :: RANDOM | e)
tests = test "Elm.Int53\n" do
    test "Int53.truncate" do
        assert "positive" <| fromInt 1 == truncate 1.5
        assert "negative" <| fromInt (-1) == truncate (-1.5)
        assert "zero" <| fromInt 0 == truncate 0.0
        assert "too big" <| top == truncate 1.0e65
        assert "too small" <| bottom == truncate (-1.0e65)

    test "Int53.fromNumber" do
        assert "nan" <| nothing == fromNumber Global.nan
        assert "fractional" <| nothing == fromNumber 2.5
        assert "too big" <| nothing == fromNumber 1.0e65
        assert "too small" <| nothing == fromNumber (-1.0e65)
        assert "just right" <| Just (fromInt 27) == fromNumber 27.0

    test "Int53.fromString" do
        assert "nan" <| nothing == fromString "not a number"
        assert "fractional" <| nothing == fromString "2.5"
        assert "too big" <| nothing == fromString "1.0e65"
        assert "too small" <| nothing == fromString "-1.0e65"
        assert "just right" <| Just (fromInt 27) == fromString "27.0"

    test "Int53.even" do
        assert "0" <| even <| fromInt 0
        assert "1" <| not <| even <| fromInt 1
        assert "2" <| even <| fromInt 2
        assert "3" <| not <| even <| fromInt 3

    test "Int53.odd" do
        assert "0" <| not <| odd <| fromInt 0
        assert "1" <| odd <| fromInt 1
        assert "2" <| not <| odd <| fromInt 2
        assert "3" <| odd <| fromInt 3

    test "Quickcheck even" $
        quickCheck quickEven

    test "Quickcheck odd" $
        quickCheck quickOdd


quickEven :: Int -> Result
quickEven a =
    Int.even a === even (fromInt a)


quickOdd :: Int -> Result
quickOdd a =
    Int.odd a === odd (fromInt a)
