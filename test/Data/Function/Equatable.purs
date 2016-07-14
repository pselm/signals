module Test.Data.Function.Equatable
    ( tests
    ) where


import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Data.Function.Equatable

import Prelude (id, bind, ($), (<<<), (>>>), (+), (*), (-), (==), (<>))


add5func :: Int -> Int
add5func = (_ + 5)


add5 :: Int ==> Int
add5 = mkEqFn (_ + 5)


times2 :: Int ==> Int
times2 = mkEqFn (_ * 2)


subtract7 :: Int ==> Int
subtract7 =
    mkEqFn \x ->
        x - 7


tests :: ∀ e. TestSuite e
tests =
    suite "Data.Function.Equatable" do
        test "identity" do
            assert "an EqFn should equal itself" $
                add5 == add5

        test "repeatability" do
            assert "two EqFn's made from the same func should be equal" $
                mkEqFn add5func == mkEqFn add5func

        test "running" do
            assert "running an EqFn should be like running the func" $
                (runEqFn add5) 2 == 7

        test "composing" do
            assert "composing equal functions should be equal" $
                (add5 >>> times2) == (add5 >>> times2)

            assert "it shouldn't matter what order you compose them in" $
                (add5 >>> (times2 >>> subtract7)) == ((add5 >>> times2) >>> subtract7)

            assert "the composed function should work" $
                runEqFn (add5 >>> times2) 3 == 16

            assert "the doubly-composed function should work" $
                runEqFn (add5 >>> (times2 >>> subtract7)) 3 == 9

            assert "the doubly-composed function should work the other way" $
                runEqFn ((add5 >>> times2) >>> subtract7) 3 == 9

        test "category laws" do
            assert "id <<< p = p" $
                id <<< subtract7 == subtract7

            assert "p <<< id = p" $
                subtract7 <<< id == subtract7

            assert "id <<< p == p >>> id" $
                id <<< subtract7 == subtract7 >>> id

            assert "id <<< p actually works" $
                runEqFn (id <<< subtract7) 9 == 2

            assert "p <<< id actually works" $
                runEqFn (subtract7 <<< id) 9 == 2
