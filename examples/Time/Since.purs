module Examples.Time.Since where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Signal (map) as Signal
import Elm.Time (every, since)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Prelude (show, bind, discard, Unit, (<<<), (<>))


main :: forall e. Eff (ref :: REF, now :: NOW, delay :: DELAY, timer :: TIMER, console :: CONSOLE | e) Unit
main =
    setup do
        timer <- every 1500.0
        sinced <- since 500.0 timer
        
        printTimer <- Signal.map (log <<< (_ <> " <- timer") <<< show) timer
        printSinced <- Signal.map (log <<< (_ <> " <- sinced") <<< show) sinced

        runSignal printTimer
        runSignal printSinced

