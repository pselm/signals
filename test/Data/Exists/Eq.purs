module Test.Data.Exists.Eq
    ( tests
    ) where


import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Data.Exists.Eq

import Prelude (bind, ($), not, (==), (/=))


tests :: ∀ e. TestSuite e
tests =
    suite "Data.Exists.Eq" do
        test "eqAny" do
            assert "Int Int true" $ 5 `eqAny` 5
            assert "Int Int false" $ not (5 `eqAny` 7)
            assert "Int String false" $ not (5 `eqAny` "five")
            assert "String String true" $ "five" `eqAny` "five"
            assert "String String false" $ not ("five" `eqAny` "six")

        test "someEq" do
            assert "Int Int true" $ someEq 5 == someEq 5
            assert "Int Int false" $ someEq 5 /= someEq 7
            assert "Int String false" $ someEq 5 /= someEq "five"
            assert "String String true" $ someEq "five" == someEq "five"
            assert "String String false" $ someEq "five" /= someEq "six"
