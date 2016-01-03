module Test.Main where

import Prelude
import Test.Unit
import Test.Unit.Console
import Control.Monad.Eff

import Test.Elm.MaybeTest ()
import Test.Elm.ListTest ()
import Test.Elm.BasicsTest ()

main :: Eff ( testOutput :: TestOutput ) Unit
main =
    runTest do
        Test.Elm.MaybeTest.tests
        Test.Elm.ListTest.tests
        Test.Elm.BasicsTest.tests
        Test.Elm.ElmBasicsTest.tests
