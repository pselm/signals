module Test.Data.Function.Equatable
    ( tests
    ) where


import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Data.Function.Equatable
import Data.Tuple (Tuple(..))

import Prelude (id, bind, ($), (<<<), (>>>), (+), (*), (-), (==), (/=), (<>), negate, (&&))


add2 :: Int -> Int
add2 a = a + 2


add2func :: EqFunc Int Int
add2func = eqFunc add2


nicerAdd2func :: Int ==> Int
nicerAdd2func = eqFunc add2


addInt :: Int -> Int -> Int
addInt = (+)


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


add5 :: Int ==> Int
add5 = eqFunc (_ + 5)


times2 :: Int ==> Int
times2 = eqFunc (_ * 2)


subtract7 :: Int ==> Int
subtract7 =
    eqFunc \a ->
        a - 7


subtract :: Int ==> Int ==> Int
subtract = eqFunc2 (-)


tests :: ∀ e. TestSuite e
tests =
    suite "Data.Function.Equatable" do
        test "identity" do
            assert "an EqFn should equal itself" $
                add2func == add2func

        test "repeatability" do
            assert "two EqFn's made from the same func should be equal" $
                add2func == nicerAdd2func

            assert "but sadly not if you don't start with the very same func" $
                eqFunc (\a -> a + 2) /= eqFunc (\a -> a + 2)

        test "running" do
            assert "running an EqFn should be like running the func" $
                add2func ~ 5 == 7

            assert "or with runEF" $
                (runEF add2func) 5 == 7

        partialApplication

        test "composing" do
            assert "composing equal functions should be equal" $
                (add5 >>> times2) == (add5 >>> times2)

            assert "but you have to compose the EqFunc's, not the funcs" $
                eqFunc ((+) 6 >>> (*) 2) /= eqFunc ((+) 6 >>> (*) 2)

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

        test "curryEF" do
            assert "uncurrying the same EqFunc should be equal" $
                uncurryEF add == uncurryEF add

            assert "uncurrying and then currying should equal the original" $
                curryEF (uncurryEF add) == add

            assert "uncurrying should actually work" $
                (uncurryEF add) ~ Tuple 3 4 == 7

            assert "uncurrying and then currying should work" $
                curryEF (uncurryEF add) ~ 3 ~ 4 == 7


x :: Int
x = 5


adder :: Int ==> (Int -> Int)
adder = eqFunc (+)


adderAgain :: Int ==> (Int -> Int)
adderAgain = eqFunc (+)


partialApplication :: ∀ e. TestSuite e
partialApplication =
    suite "Partial Application" do
        test "eqFunc" do
            assert "eqFunc should sort of work with 2 param functions" $
                adder == adderAgain

            assert "but not this way" $
                eqFunc (\a b -> (a :: Int) + b) /= eqFunc (\a b -> (a :: Int) + b)

        test "eqFunc2" do
            assert "a function partially applied twice with the same parameter should be equal" $
                add ~ 2 == add ~ 2

            assert "or from scratch" $
                (eqFunc2 addInt) ~ 2 == (eqFunc2 addInt) ~ 2

            assert "or in a lambda-like way" $
                (eqFunc2 addInt) ~ x == (eqFunc2 addInt) ~ x &&
                (eqFunc2 addInt) ~ x == (eqFunc2 addInt) ~ 5 &&
                (eqFunc2 addInt) ~ x /= (eqFunc2 addInt) ~ 7

            assert "a function partially applied twice with a different parameter should not be equal" $
                add ~ 2 /= add ~ 3

            assert "or from scratch" $
                (eqFunc2 addInt) ~ 2 /= (eqFunc2 addInt) ~ 3

            assert "a partially applied function should actually work" $
                add ~ 2 ~ 3 == 5

            assert "or using runEF" $
                (runEF2 add) 2 3 == 5

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
