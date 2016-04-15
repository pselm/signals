module Examples.Window.FullScreen where


import Elm.Window
import Elm.Signal (DELAY, Signal, setup, runSignal, map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans (lift)
import Data.Date (Now)
import DOM.Timer (Timer)
import DOM (DOM)
import Prelude (class Show, show, bind, Unit, (<<<), (++), ($))


-- I haven't implemented HTML stuff yet, so just write things to the console.log for the moment
logWindow ::
    ∀ e m a. (Show a, MonadEff (ref :: REF | e) m) =>
    String -> WindowCallback m (Signal a) -> WindowCallback m Unit

logWindow label signal = do
    sig <- signal
    printer <- lift $ map (log <<< (_ ++ (" <- " ++ label)) <<< show) sig
    lift $ runSignal printer


main :: ∀ e. Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer, dom :: DOM | e) Unit
main =
    setup do
        setupGlobalWindow do
            logWindow "width" width
            logWindow "height" height
