module Examples.Time.Delay where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Signal (map) as Signal
import Elm.Time.Signal (every, delay)
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
        delayed <- delay 500.0 timer
        
        printTimer <- Signal.map (log <<< (_ <> " <- timer") <<< show) timer
        printDelayed <- Signal.map (log <<< (_ <> " <- delayed") <<< show) delayed

        runSignal printTimer
        runSignal printDelayed

