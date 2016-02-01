module Test.QuickCheck.Laws.Data.DivisionRing where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/DivisionRing.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Multiplicative inverse: `(one / x) * x = one`
checkDivisionRing :: forall e a. (DivisionRing a, Arbitrary a, Eq a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkDivisionRing _ = do
    test "Checking 'Multiplicative inverse' law for DivisionRing" $
        quickCheck' 1000 multiplicativeInverse

    where
        multiplicativeInverse :: a -> Boolean
        multiplicativeInverse x = (x == zero) || ((one / x) * x == one)
