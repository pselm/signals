module Test.Main where

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.AVar (AVAR)
import Elm.Signal (DELAY)

import Test.Elm.SignalTest as SignalTest

import Prelude (Unit)


main :: Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , delay :: DELAY
    , now :: NOW
    , ref :: REF
    ) Unit

main =
    runTest do
        SignalTest.tests
