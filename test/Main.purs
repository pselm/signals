module Test.Main where

import Prelude (Unit, bind)
import Test.Unit (TIMER, runTest)
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Elm.Signal (DELAY)
import Data.Date (Now)

main :: forall e. Eff
    ( testOutput :: TESTOUTPUT
    , avar :: AVAR
    , timer :: TIMER
    , random :: RANDOM
    , err :: EXCEPTION
    , console :: CONSOLE
    , ref :: REF
    , delay :: DELAY
    , now :: Now
    | e) Unit

main =
    runTest do
        Test.Elm.TextTest.tests
        Test.Elm.ColorTest.tests
        Test.Elm.MaybeTest.tests
        Test.Elm.ListTest.tests
        Test.Elm.ElmListTest.tests
        Test.Elm.BasicsTest.tests
        Test.Elm.ElmBasicsTest.tests
        Test.Elm.BitwiseTest.tests
        Test.Elm.CharTest.tests
        Test.Elm.ResultTest.tests
        Test.Elm.DictTest.tests
        Test.Elm.StringTest.tests
        Test.Elm.SetTest.tests
        Test.Elm.ArrayTest.tests
        Test.Elm.DateTest.tests
        Test.Elm.TimeTest.tests
        Test.Elm.RandomTest.tests
        Test.Elm.Int53Test.tests
        Test.Elm.RegexTest.tests
        Test.Elm.TaskTest.tests
        Test.Elm.SignalTest.tests
        Test.Elm.JsonTest.tests
        Test.Elm.TrampolineTest.tests
