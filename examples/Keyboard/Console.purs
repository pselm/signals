module Examples.Keyboard.Console where


import Elm.Signal (DELAY, Signal, setup, runSignal, map)
import Elm.Keyboard 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Trans (lift)
import DOM (DOM)
import DOM.Timer (Timer)
import Prelude (class Show, show, bind, Unit, (<<<), (<>), ($))


-- I haven't implemented HTML stuff yet, so just write things to the console.log for the moment
logKeyboard :: 
    ∀ e m a. (Show a, MonadEff (ref :: REF | e) m) =>
    String -> Keyboard m (Signal a) -> Keyboard m Unit

logKeyboard label signal = do
    sig <- signal
    printer <- lift $ map (log <<< (_ <> (" <- " <> label)) <<< show) sig
    lift $ runSignal printer


main :: ∀ e. Eff (ref :: REF, now :: NOW, delay :: DELAY, console :: CONSOLE, timer :: Timer, dom :: DOM | e) Unit
main =
    setup do
        setupKeyboard do
            logKeyboard "arrows" arrows
            logKeyboard "wasd" wasd
            logKeyboard "enter" enter
            logKeyboard "space" space
            logKeyboard "ctrl" ctrl
            logKeyboard "shift" shift
            logKeyboard "alt" alt
            logKeyboard "meta" meta
            logKeyboard "keysDown" keysDown
            logKeyboard "presses" presses
            logKeyboard "isDown 88 (x)" (isDown 88)
