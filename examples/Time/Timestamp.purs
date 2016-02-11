module Examples.Time.Timestamp where


import Elm.Signal (DELAY, setup, timestamp, runSignal)
import Elm.Time (every)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Data.Date (Now)
import Prelude (show, bind, Unit, (<<<))


main :: forall e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: TIMER | e) Unit
main =
    setup do
        timer <- every 1500.0
        ts <- timestamp timer
        runner <- Elm.Signal.map (log <<< show) ts
        runSignal runner


