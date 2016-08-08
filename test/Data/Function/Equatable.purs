module Test.Data.Function.Equatable
    ( tests
    ) where


import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Data.Function.Equatable

import Prelude (id, bind, ($), (<<<), (>>>), (+), (*), (-), (==), (/=), (<>), negate)


add5func :: Int -> Int
add5func = (_ + 5)


add5 :: Int ==> Int
add5 = eqFunc (_ + 5)


add :: Int ==> Int ==> Int
add = eqFunc2 (+)


add3 :: Int ==> Int ==> Int ==> Int
add3 =
    eqFunc3 \a b c ->
        a + b + c

add4 :: Int ==> Int ==> Int ==> Int ==> Int
add4 =
    eqFunc4 \a b c d ->
        a + b + c + d


times2 :: Int ==> Int
times2 = eqFunc (_ * 2)


subtract7 :: Int ==> Int
subtract7 =
    eqFunc \x ->
        x - 7


subtract :: Int ==> Int ==> Int
subtract = eqFunc2 (-)


tests :: ∀ e. TestSuite e
tests =
    suite "Data.Function.Equatable" do
        test "identity" do
            assert "an EqFn should equal itself" $
                add5 == add5

        test "repeatability" do
            assert "two EqFn's made from the same func should be equal" $
                eqFunc add5func == eqFunc add5func

        test "running" do
            assert "running an EqFn should be like running the func" $
                add5 ~ 2 == 7

        test "composing" do
            assert "composing equal functions should be equal" $
                (add5 >>> times2) == (add5 >>> times2)

            assert "it shouldn't matter what order you compose them in" $
                (add5 >>> (times2 >>> subtract7)) == ((add5 >>> times2) >>> subtract7)

            assert "the composed function should work" $
                (add5 >>> times2) ~ 3 == 16

            assert "the doubly-composed function should work" $
                (add5 >>> (times2 >>> subtract7)) ~ 3 == 9

            assert "the doubly-composed function should work the other way" $
                ((add5 >>> times2) >>> subtract7) ~ 3 == 9

        test "category laws" do
            assert "id <<< p = p" $
                id <<< subtract7 == subtract7

            assert "p <<< id = p" $
                subtract7 <<< id == subtract7

            assert "id <<< p == p >>> id" $
                id <<< subtract7 == subtract7 >>> id

            assert "id <<< p actually works" $
                (id <<< subtract7) ~ 9 == 2

            assert "p <<< id actually works" $
                (subtract7 <<< id) ~ 9 == 2

        test "eqFunc2" do
            assert "a function partially applied twice with the same parameter should be equal" $
                add ~ 2 == add ~ 2

            assert "a function partially applied twice with a different parameter should not be equal" $
                add ~ 2 /= add ~ 3

            assert "a partially applied function should actually work" $
                add ~ 2 ~ 3 == 5

        test "eqFunc3" do
            assert "a function partially applied twice with the same parameter should be equal" $
                add3 ~ 2 == add3 ~ 2

            assert "a function partially applied twice with the same two parameters should be equal" $
                add3 ~ 2 ~ 5 == add3 ~ 2 ~ 5

            assert "a function partially applied twice with a different parameter should not be equal" $
                add3 ~ 2 /= add3 ~ 3

            assert "a function partially applied twice with two different parameters should not be equal" $
                add3 ~ 2 ~  3 /= add3 ~ 3 ~ 4

            assert "a partially applied function should actually work" $
                add3 ~ 2 ~ 3 ~ 4 == 9

        test "eqFunc4" do
            assert "a function partially applied twice with the same parameter should be equal" $
                add4 ~ 2 == add4 ~ 2

            assert "a function partially applied twice with the same two parameters should be equal" $
                add4 ~ 2 ~ 5 == add4 ~ 2 ~ 5

            assert "a function partially applied twice with the same three parameters should be equal" $
                add4 ~ 2 ~ 5 ~ 7 == add4 ~ 2 ~ 5 ~ 7

            assert "a function partially applied twice with a different parameter should not be equal" $
                add4 ~ 2 /= add4 ~ 3

            assert "a function partially applied twice with two different parameters should not be equal" $
                add4 ~ 2 ~ 3 /= add4 ~ 3 ~ 4

            assert "a function partially applied twice with three different parameters should not be equal" $
                add4 ~ 2 ~ 3 ~ 4 /= add4 ~ 3 ~ 4 ~ 5

            assert "a partially applied function should actually work" $
                add4 ~ 2 ~ 3 ~ 4 ~ 5 == 14

        test "constEF" do
            assert "two constEF functions with the same result should be equal" $
                constEF 7 == constEF 7

            assert "two constEF functions with different results should be different" $
                constEF 12 /= constEF 11

            assert "constEF should actually work" $
                (constEF 17) ~ 12 == 17

        test "flipEF" do
            assert "two flipped functions should be equal to each other" $
                flipEF subtract == flipEF subtract

            assert "flipping something twice should equal itself" $
                flipEF (flipEF (subtract)) == subtract

            assert "flipEF actually works" $
                (flipEF subtract) ~ 7 ~ 5 == (-2)

            assert "a function flipped twice actually works" $
                (flipEF (flipEF subtract)) ~ 7 ~ 5 == 2
