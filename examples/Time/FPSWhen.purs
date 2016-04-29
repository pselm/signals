module Examples.Time.FPSWhen where


import Elm.Signal (DELAY, setup, current, send, runSignal, mailbox)
import Elm.Time (fpsWhen)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.Timer (Timer, interval)
import Data.Date (Now)
import Prelude (show, bind, Unit, not, ($))


main :: forall e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) Unit
main =
    setup do
        let
            logger time =
                log (show time)

        trigger <- mailbox false

        -- Turn it on and off every so often
        liftEff $ interval 5000 do
            on <- current trigger.signal
            send trigger.address (not on)

        timer <- fpsWhen 1.0 trigger.signal
        runner <- Elm.Signal.map logger timer
        runSignal runner


