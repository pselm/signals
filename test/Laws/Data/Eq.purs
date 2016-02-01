module Test.QuickCheck.Laws.Data.Eq where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/Eq.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
checkEq :: forall e a. (Arbitrary a, Eq a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkEq _ = do

    test "Checking 'Reflexivity' law for Eq" $
        quickCheck' 1000 reflexivity

    test "Checking 'Symmetry' law for Eq" $
        quickCheck' 1000 symmetry

    test "Checking 'Transitivity' law for Eq" $
        quickCheck' 1000 transitivity

    test "Checking 'Negation' law for Eq" $
        quickCheck' 1000 negation

    where
        reflexivity :: a -> Boolean
        reflexivity x = (x == x) == true

        symmetry :: a -> a -> Boolean
        symmetry x y = (x == y) == (y == x)

        transitivity :: a -> a -> a -> Boolean
        transitivity x y z = if (x == y) && (y == z) then x == z else true

        negation :: a -> a -> Boolean
        negation x y = (x /= y) == not (x == y)
