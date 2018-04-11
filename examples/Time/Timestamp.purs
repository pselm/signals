module Examples.Time.Timestamp where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Signal (map) as Signal
import Elm.Time.Signal (every, timestamp)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Prelude (show, bind, Unit, (<<<))


main :: forall e. Eff (ref :: REF, now :: NOW, delay :: DELAY, timer :: TIMER, console :: CONSOLE | e) Unit
main =
    setup do
        timer <- every 1500.0
        ts <- timestamp timer
        runner <- Signal.map (log <<< show) ts
        runSignal runner


