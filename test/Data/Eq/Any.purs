module Test.Data.Eq.Any
    ( tests
    ) where


import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Data.Eq.Any

import Prelude (bind, ($), not, (==), (/=))


tests :: ∀ e. TestSuite e
tests =
    suite "Data.Eq.Any" do
        test "eqAny" do
            assert "Int Int true" $ 5 `eqAny` 5
            assert "Int Int false" $ not (5 `eqAny` 7)
            assert "Int String false" $ not (5 `eqAny` "five")
            assert "String String true" $ "five" `eqAny` "five"
            assert "String String false" $ not ("five" `eqAny` "six")

        test "anyEq" do
            assert "Int Int true" $ anyEq 5 == anyEq 5
            assert "Int Int false" $ anyEq 5 /= anyEq 7
            assert "Int String false" $ anyEq 5 /= anyEq "five"
            assert "String String true" $ anyEq "five" == anyEq "five"
            assert "String String false" $ anyEq "five" /= anyEq "six"
