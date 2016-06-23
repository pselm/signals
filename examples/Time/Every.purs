module Examples.Time.Every where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Signal (map) as Signal
import Elm.Time (every)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import DOM.Timer (Timer)
import Prelude (show, bind, Unit)


main :: forall e. Eff (ref :: REF, now :: NOW, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) Unit
main =
    setup do
        let
            logger time =
                log (show time)

        timer <- every 5000.0
        runner <- Signal.map logger timer
        runSignal runner


