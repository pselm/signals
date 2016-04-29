module Examples.Time.FPS where


import Elm.Signal (DELAY, setup, runSignal)
import Elm.Time (fps)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.Timer (Timer)
import Data.Date (Now)
import Prelude (show, bind, Unit)


main :: forall e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) Unit
main =
    setup do
        let
            logger time =
                log (show time)

        timer <- fps 1.0 
        runner <- Elm.Signal.map logger timer
        runSignal runner


