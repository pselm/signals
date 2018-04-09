module Examples.Time.FPSWhen where


import Elm.Signal (DELAY, setup, current, send, runSignal, mailbox)
import Elm.Signal (map) as Signal
import Elm.Time (fpsWhen)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Prelude (show, bind, discard, void, Unit, not, ($))


main :: forall e. Eff (ref :: REF, now :: NOW, delay :: DELAY, timer :: TIMER, console :: CONSOLE | e) Unit
main =
    setup do
        let
            logger time =
                log (show time)

        trigger <- mailbox false

        -- Turn it on and off every so often
        liftEff $ void $ setInterval 5000 do
            on <- current trigger.signal
            send trigger.address (not on)

        timer <- fpsWhen 1.0 trigger.signal
        runner <- Signal.map logger timer
        runSignal runner


