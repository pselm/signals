module Test.QuickCheck.Laws.Data.Semiring where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/Semiring.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihiliation: `zero * a = a * zero = zero`
checkSemiring :: forall e a. (Semiring a, Arbitrary a, Eq a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkSemiring _ = do

    test "Checking 'Associativity' law for Semiring addition" $
        quickCheck' 1000 associativeAddition

    test "Checking 'Identity' law for Semiring addition" $
        quickCheck' 1000 identityAddition

    test "Checking 'Commutative' law for Semiring addition" $
        quickCheck' 1000 commutativeAddition

    test "Checking 'Associativity' law for Semiring multiplication" $
        quickCheck' 1000 associativeMultiplication

    test "Checking 'Identity' law for Semiring multiplication" $
        quickCheck' 1000 identityMultiplication

    test "Checking 'Left distribution' law for Semiring" $
        quickCheck' 1000 leftDistribution

    test "Checking 'Right distribution' law for Semiring" $
        quickCheck' 1000 rightDistribution

    where
        associativeAddition :: a -> a -> a -> Boolean
        associativeAddition a b c = (a + b) + c == a + (b + c)

        identityAddition :: a -> Boolean
        identityAddition a = (zero + a == a) && (a + zero == a)

        commutativeAddition :: a -> a -> Boolean
        commutativeAddition a b = a + b == b + a

        associativeMultiplication :: a -> a -> a -> Boolean
        associativeMultiplication a b c = (a * b) * c == a * (b * c)

        identityMultiplication :: a -> Boolean
        identityMultiplication a = (one * a == a) && (a * one == a)

        leftDistribution :: a -> a -> a -> Boolean
        leftDistribution a b c = a * (b + c) == (a * b) + (a * c)

        rightDistribution :: a -> a -> a -> Boolean
        rightDistribution a b c = (a + b) * c == (a * c) + (b * c)

        annihiliation :: a -> Boolean
        annihiliation a = (a * zero == zero) && (zero * a == zero)
