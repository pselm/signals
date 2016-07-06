module Test.Main where

import Test.Unit (TIMER)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.AVar (AVAR)
import Elm.Signal (DELAY)

import DOM (DOM)
import DOM.JSDOM (JSDOM)
import Graphics.Canvas (CANVAS)

import Test.Elm.TextTest as TextTest
import Test.Elm.ColorTest as ColorTest
import Test.Elm.TimeTest as TimeTest
import Test.Elm.TaskTest as TaskTest
import Test.Elm.SignalTest as SignalTest
import Test.Elm.VirtualDomTest as VirtualDomTest

import Prelude (Unit, bind)


main :: Eff
    ( testOutput :: TESTOUTPUT
    , avar :: AVAR
    , timer :: TIMER
    , random :: RANDOM
    , err :: EXCEPTION
    , console :: CONSOLE
    , ref :: REF
    , delay :: DELAY
    , now :: NOW
    , canvas :: CANVAS
    , dom :: DOM
    , jsdom :: JSDOM
    ) Unit

main =
    runTest do
        TextTest.tests
        ColorTest.tests
        TimeTest.tests
        TaskTest.tests
        SignalTest.tests
        VirtualDomTest.tests
