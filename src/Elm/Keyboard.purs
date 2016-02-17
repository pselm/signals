
-- | Library for working with keyboard input.

module Elm.Keyboard
    ( XY, arrows, wasd
    , enter, space, ctrl, shift, alt, meta
    , isDown, keysDown, presses

    -- This is not part of the Elm API. Some of this
    -- should be newyptes, so we don't need to export.
    , Keyboard, KeyboardState, Model, KeyCode
    , setupKeyboard
    ) where


import Data.Set (Set, insert, delete, member)
import Elm.Basics (Bool)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (reader)
import Control.Monad.State.Trans (StateT)
import Prelude (class Eq, Unit, unit, bind, ($), const, pure, (>>=), (-), (&&), (==), class Show, show, (++))
import Elm.Signal (DELAY, Signal, GraphState, Graph, foldp, map, dropRepeats, Mailbox, mailbox, send, mergeMany)

import Control.Monad.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Date (Now)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import DOM.Event.Types (EventType, Event)
import DOM.Event.EventTypes (blur, keypress, keydown, keyup)
import DOM.Event.EventTarget (eventListener, addEventListener)


-- | The Keyboard API uses some hidden state, managed by this type.
type Keyboard m a = ReaderT KeyboardState (StateT Graph m) a


-- Keyboard state
type KeyboardState =
    { model :: Signal Model
    , keysDown :: Signal (Set KeyCode)
    }


makeKeyboardState ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m KeyboardState

makeKeyboardState = do
    modelSignal <- makeModel
    keysDownSignal <- makeKeysDown modelSignal

    pure
        { model: modelSignal
        , keysDown: keysDownSignal
        }


-- | Setup keyboard signals.
-- |
-- |     setupKeyboard do
-- |         altSignal <- alt
setupKeyboard ::
    ∀ e m a. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    Keyboard m a -> GraphState m a

setupKeyboard cb =
    makeKeyboardState >>= runReaderT cb


-- | Key codes for different layouts. You can set it up to be WASD, arrow keys, etc.
-- |
-- |     arrowKeys = { up: 38, down: 40, left: 37, right: 39 }
-- |     wasdKeys = { up: 87, down: 83, left: 65, right: 68 }
type Directions =
    { up :: KeyCode
    , down :: KeyCode
    , left :: KeyCode
    , right :: KeyCode
    }


newtype XY = XY
    { x :: Int
    , y :: Int
    }

instance eqXY :: Eq XY where
    eq (XY a) (XY b) =
        a.x == b.x && a.y == b.y

instance showXY :: Show XY where
    show (XY a) =
        "{x: " ++ show a.x ++ ", y: " ++ show a.y ++ "}"


-- | Extract an x and y value representing directions from a set of key codes
-- | that are currently pressed. For example, you can use this to define `wasd`
-- | like this:
-- |
-- |     wasd : Signal { x : Int, y : Int }
-- |     wasd =
-- |         Signal.map (toXY { up = 87, down = 83, left = 65, right = 68 }) keysDown
toXY :: Directions -> Set KeyCode -> XY
toXY {up, down, left, right} keyCodes =
    let
        is keyCode =
            if member keyCode keyCodes
                then 1
                else 0

    in
        XY
            { x: is right - is left
            , y: is up - is down
            }


-- | A signal of records indicating which arrow keys are pressed.
-- |
-- | * `{ x = 0, y = 0 }` when pressing no arrows.
-- | * `{ x =-1, y = 0 }` when pressing the left arrow.
-- | * `{ x = 1, y = 1 }` when pressing the up and right arrows.
-- | * `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
arrows ::
    ∀ e m. (MonadEff (ref :: REF | e) m) =>
    Keyboard m (Signal XY)

arrows = do
    kd <- reader _.keysDown
    lift $
        dropMap
            (toXY {up: 38, down: 40, left: 37, right: 39})
            kd


-- | Just like the arrows signal, but this uses keys w, a, s, and d,
-- | which are common controls for many computer games.
wasd ::
    ∀ e m. (MonadEff (ref :: REF | e) m) =>
    Keyboard m (Signal XY)

wasd = do
    kd <- reader _.keysDown
    lift $
        dropMap
            (toXY {up: 87, down: 83, left: 65, right: 68})
            kd


-- | Whether an arbitrary key is pressed.
isDown ::
    ∀ e m. (MonadEff (ref :: REF | e) m) =>
    KeyCode -> Keyboard m (Signal Bool)

isDown keyCode = do
    kd <- reader _.keysDown
    lift $ dropMap (member keyCode) kd


-- | Whether the alt key is pressed.
alt :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
alt = do
    model <- reader _.model
    lift $ dropMap _.alt model


-- | Whether the ctrl key is pressed.
ctrl :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
ctrl = isDown 17


-- | Whether the meta key is pressed.
-- |
-- | The meta key is the Windows key on Windows and the Command key on Mac.
meta :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
meta = do
    model <- reader _.model
    lift $ dropMap _.meta model


-- | Whether the shift key is pressed.
shift :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
shift = isDown 16


-- | Whether the space key is pressed.
space :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
space = isDown 32


-- Whether the enter key is pressed.
enter :: ∀ e m. (MonadEff (ref :: REF | e) m) => Keyboard m (Signal Bool)
enter = isDown 13


-- | Set of keys that are currently down.
keysDown :: ∀ e m. (MonadEff (ref :: REF | e) m) =>  Keyboard m (Signal (Set KeyCode))
keysDown = reader _.keysDown


-- | The latest key that has been pressed.
presses ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    Keyboard m (Signal KeyCode)

presses =
    lift do
        p <- _presses
        map _.keyCode p


-- MANAGE RAW STREAMS

type Model =
    { alt :: Boolean
    , meta :: Boolean
    , keyCodes :: Set KeyCode
    }


empty :: Model
empty =
    { alt: false
    , meta: false
    , keyCodes: Data.Set.empty
    }


update :: KeyEvent -> Model -> Model
update event model =
    case event of
        Down info ->
            { alt: info.alt
            , meta: info.meta
            , keyCodes: insert info.keyCode model.keyCodes
            }

        Up info ->
            { alt: info.alt
            , meta: info.meta
            , keyCodes: delete info.keyCode model.keyCodes
            }

        Blur ->
            empty


makeModel ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m (Signal Model)

makeModel = do
    raw <- rawEvents
    foldp update empty raw


makeKeysDown :: ∀ e m. (MonadEff (ref :: REF | e) m) => Signal Model -> GraphState m (Signal (Set KeyCode))
makeKeysDown model =
    dropMap _.keyCodes model


data KeyEvent
    = Up EventInfo
    | Down EventInfo
    | Blur


rawEvents ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m (Signal KeyEvent)

rawEvents = do
    upSig <- ups >>= map Up
    downSig <- downs >>= map Down
    blurSig <- blurs >>= map (const Blur)

    mergeMany
        ( upSig
        : downSig
        : blurSig
        : Nil
        )


dropMap ::
    ∀ e m a b. (MonadEff (ref :: REF | e) m, Eq b) =>
    (a -> b) -> Signal a -> GraphState m (Signal b)

dropMap func signal =
    map func signal >>= dropRepeats


type KeyCode = Int


type EventInfo =
    { alt :: Bool
    , meta :: Bool
    , keyCode :: KeyCode
    }


keyListener :: ∀ e. Mailbox EventInfo -> Event -> Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE | e) Unit
keyListener mbox event = do
    let
        eventInfo = do
            let foreignEvent = toForeign event

            altProp <- readProp "altKey" foreignEvent
            metaProp <- readProp "metaKey" foreignEvent
            keyCodeProp <- readProp "keyCode" foreignEvent

            pure
                { alt: altProp
                , meta: metaProp
                , keyCode: keyCodeProp
                }

    case eventInfo of
        Right info ->
            send mbox.address info

        Left err ->
            print err


keyEvent ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    EventType -> GraphState m (Signal EventInfo)

keyEvent event = do
    mbox <- mailbox
        { alt: false
        , meta: false
        , keyCode: 0
        }

    doc <- liftEff $
        window >>= document

    liftEff $
        addEventListener
            event
            (eventListener $ keyListener mbox)
            false
            (htmlDocumentToEventTarget doc)

    pure mbox.signal


_presses ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m (Signal EventInfo)

_presses = keyEvent keypress


downs ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m (Signal EventInfo)

downs = keyEvent keydown


ups ::
    ∀ e m. (MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m) =>
    GraphState m (Signal EventInfo)

ups = keyEvent keyup


-- Emits whenever the window object gets a 'blur' event
blurs ::
    ∀ e m. (MonadEff (dom :: DOM, ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE | e) m) =>
    GraphState m (Signal Unit)

blurs = do
    win <- liftEff $ window
    mbox <- mailbox unit

    let
        listener =
            eventListener $
                const $
                    send mbox.address unit

    liftEff $
        addEventListener blur listener false (windowToEventTarget win)

    pure mbox.signal

