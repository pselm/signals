module Test.QuickCheck.Laws.Data.Ord where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/Ord.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
checkOrd :: forall e a. (Arbitrary a, Ord a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkOrd _ = do

    test "Checking 'Reflexivity' law for Ord" $
        quickCheck' 1000 reflexivity

    test "Checking 'Antisymmetry' law for Ord" $
        quickCheck' 1000 antisymmetry

    test "Checking 'Transitivity' law for Ord" $
        quickCheck' 1000 transitivity

    where
        reflexivity :: a -> Boolean
        reflexivity a = a <= a

        antisymmetry :: a -> a -> Boolean
        antisymmetry a b = if (a <= b) && (b <= a) then a == b else a /= b

        transitivity :: a -> a -> a -> Boolean
        transitivity a b c = if (a <= b) && (b <= c) then a <= c else true
