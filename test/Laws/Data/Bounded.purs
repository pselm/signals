module Test.QuickCheck.Laws.Data.Bounded where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/Bounded.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Ordering: `bottom <= a <= top`
checkBounded :: forall e a. (Arbitrary a, Bounded a, Ord a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkBounded _ = do

    test "Checking 'Ordering' law for Bounded" $
        quickCheck' 1000 ordering

    where
        ordering :: a -> Boolean
        ordering a = bottom <= a && a <= top
