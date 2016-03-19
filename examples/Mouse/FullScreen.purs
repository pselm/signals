module Examples.Mouse.FullScreen where


import Elm.Mouse
import Elm.Signal (DELAY, Signal, setup, runSignal, map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Trans (lift)
import Data.Date (Now)
import DOM (DOM)
import Prelude (class Show, show, bind, Unit, (<<<), (++), ($))


-- I haven't implemented HTML stuff yet, so just write things to the console.log for the moment
logMouse ::
    ∀ e m a. (Show a, MonadEff (ref :: REF | e) m) =>
    String -> Mouse m (Signal a) -> Mouse m Unit

logMouse label signal = do
    sig <- signal
    printer <- lift $ map (log <<< (++ (" <- " ++ label)) <<< show) sig
    lift $ runSignal printer


main :: ∀ e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: TIMER, dom :: DOM | e) Unit
main =
    setup do
        setupGlobalMouse do
            logMouse "x" x
            logMouse "y" y
            logMouse "isDown" isDown
            logMouse "clicks" clicks
