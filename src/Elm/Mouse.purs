
-- | Library for working with mouse input.

module Elm.Mouse
    ( position, x, y
    , isDown, clicks
    -- This is not part of the Elm API, and may need changes as I figure
    -- out how best to do this.
    , Mouse, MousePosition, MouseState
    , setupMouse, setupGlobalMouse
    ) where


import Elm.Basics (Bool)
import Elm.Signal (Signal, DELAY, GraphState, Graph, send, mailbox, map)

import Prelude (Unit, pure, ($), bind, unit, const, (<<<), (>>=))
import Data.Date (Now)
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (reader)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToEventTarget)
import DOM.Event.Types (EventTarget, Event)
import DOM.Event.EventTypes (mouseup, mousedown, mousemove, click)
import DOM.Event.EventTarget (eventListener, addEventListener)


-- | The Mouse API uses some hidden state, managed by this type.
type Mouse m a = ReaderT MouseState (StateT Graph m) a


-- Mouse position
type MousePosition =
    { x :: Int
    , y :: Int
    }


type MouseState =
    { position :: Signal MousePosition
    , node :: EventTarget
    }


-- | Turns the event into a MousePosition, where the MousePosition
-- | is relative to the target.
foreign import getOffsetXY :: ∀ e. Event -> Eff (dom :: DOM | e) MousePosition


makeMouseState :: ∀ e m.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    EventTarget -> GraphState m MouseState

makeMouseState node = do
    mbox <- mailbox {x: 0, y: 0}

    let
        listener =
            eventListener $ \event ->
                getOffsetXY event >>= send mbox.address

    liftEff $
        addEventListener mousemove listener false node

    pure
        { position: mbox.signal
        , node: node
        }


-- | Setup mouse signals, where the mouse is relative to a particular node.
-- |
-- |     setupMouse node do
-- |         altSignal <- alt
setupMouse :: ∀ e m a.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    EventTarget -> Mouse m a -> GraphState m a

setupMouse node cb =
    makeMouseState node >>= runReaderT cb


-- | Setup mouse signals, where the mouse is relative to the window.
-- |
-- |     setupMouse node do
-- |         altSignal <- alt
setupGlobalMouse :: ∀ e m a.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    Mouse m a -> GraphState m a

setupGlobalMouse cb = do
    node <- liftEff $
        window >>= document >>= body

    case toMaybe node of
        Just n -> setupMouse (htmlElementToEventTarget n) cb
        Nothing -> unsafeCrashWith "No document.body to be found"


-- | The current mouse position.
position :: ∀ e m. (MonadEff (ref :: REF | e) m) => Mouse m (Signal MousePosition)
position = reader _.position


-- | The current x-coordinate of the mouse.
x :: ∀ e m. (MonadEff (ref :: REF | e) m) => Mouse m (Signal Int)
x = position >>= lift <<< map _.x


-- | The current y-coordinate of the mouse.
y :: ∀ e m. (MonadEff (ref :: REF | e) m) => Mouse m (Signal Int)
y = position >>= lift <<< map _.y


-- | The current state of the mouse.
-- | True when any mouse button is down, and false otherwise.
isDown :: ∀ e m.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    Mouse m (Signal Bool)

isDown = do
    node <- reader _.node
    mbox <- lift $ mailbox false

    let
        down =
            eventListener $
                const $
                    send mbox.address true

        up =
            eventListener $
                const $
                    send mbox.address false

    liftEff do
        addEventListener mousedown down false node
        addEventListener mouseup up false node

    pure mbox.signal


-- | Always equal to unit. Event triggers on every mouse click.
clicks :: ∀ e m.
    (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    Mouse m  (Signal Unit)

clicks = do
    node <- reader _.node
    mbox <- lift $ mailbox unit

    let
        listener =
            eventListener $
                const $
                    send mbox.address unit

    liftEff $
        addEventListener click listener false node

    pure mbox.signal
