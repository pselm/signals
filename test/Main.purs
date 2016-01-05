module Test.Main where

import Prelude
import Test.Unit
import Test.Unit.Console
import Control.Monad.Eff

main :: Eff ( testOutput :: TestOutput ) Unit
main =
    runTest do
        Test.Elm.MaybeTest.tests
        Test.Elm.ListTest.tests
        Test.Elm.BasicsTest.tests
        Test.Elm.ElmBasicsTest.tests
        Test.Elm.BitwiseTest.tests
        Test.Elm.CharTest.tests
        Test.Elm.ResultTest.tests
        Test.Elm.DictTest.tests
        Test.Elm.StringTest.tests
