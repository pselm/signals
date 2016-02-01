module Test.QuickCheck.Laws.Data.Ring where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/Ring.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Additive inverse: `a + (-a) = (-a) + a = zero`
checkRing :: forall e a. (Ring a, Arbitrary a, Eq a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkRing _ = do

    test "Checking 'Additive inverse' law for Ring" $
        quickCheck' 1000 additiveInverse

    where
        additiveInverse :: a -> Boolean
        additiveInverse a = a + (-a) == zero
