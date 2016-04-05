module Examples.Time.Since where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Time (every, since)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import DOM.Timer (Timer)
import Data.Date (Now)
import Prelude (show, bind, Unit, (<<<), (++))


main :: forall e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) Unit
main =
    setup do
        timer <- every 1500.0
        sinced <- since 500.0 timer
        
        printTimer <- Elm.Signal.map (log <<< (++ " <- timer") <<< show) timer
        printSinced <- Elm.Signal.map (log <<< (++ " <- sinced") <<< show) sinced

        runSignal printTimer
        runSignal printSinced

