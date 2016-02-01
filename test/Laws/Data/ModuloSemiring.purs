module Test.QuickCheck.Laws.Data.ModuloSemiring where

-- This is adapted from https://github.com/garyb/purescript-quickcheck-laws/blob/master/src/Test/QuickCheck/Laws/Data/ModuloSemiring.purs

import Prelude

import Type.Proxy (Proxy())
import Control.Monad.Eff.Random (RANDOM())

import Test.Unit
import Test.Unit.QuickCheck (quickCheck')

import Test.QuickCheck.Arbitrary (Arbitrary)


-- | - Remainder: ```a / b * b + (a `mod` b) = a```
checkModuloSemiring :: forall e a. (ModuloSemiring a, Arbitrary a, Eq a) => Proxy a -> TestUnit (random :: RANDOM | e)
checkModuloSemiring _ = do

    test "Checking 'Remainder' law for ModuloSemiring" $
        quickCheck' 1000 remainder

    where
        remainder :: a -> a -> Boolean
        remainder a b = a / b * b + (a `mod` b) == a
