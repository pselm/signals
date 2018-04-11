
-- | The Elm 0.16-specific parts of Elm's `Time` package.
-- |
-- | This supplements `Elm.Time` from `purescript-elm-compat` by adding
-- | the `Signal`-oriented functions from Elm 0.16.

module Elm.Time.Signal
    ( module Virtual
    , fps, fpsWhen, every
    , delay, since
    ) where

import Elm.Signal (timestamp) as Virtual

-- Should we re-export all of Elm.Time? I'll see what it feels like to not do
-- that at first.

import Elm.Basics (Float, Bool)
import Elm.Signal (Signal, dropRepeats, send, mailbox, constant, current, output, foldp, merge, DELAY, GraphState)
import Elm.Signal (map) as Signal
import Elm.Time hiding (now) 

import Data.Int (round)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))

import Control.Monad.Eff.Timer (TIMER, IntervalId, setInterval, clearInterval, setTimeout)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Console (CONSOLE)

import Prelude ((/), ($), (<$>), (<<<), bind, discard, pure, (-), unit, (>>=), void, const, negate, (+), (/=))


-- | Takes desired number of frames per second (FPS). The resulting signal
-- | gives a sequence of time deltas as quickly as possible until it reaches
-- | the desired FPS. A time delta is the time between the last frame and the
-- | current frame.
fps ::
    forall e m. (MonadEff (ref :: REF, now :: NOW, delay :: DELAY, console :: CONSOLE, timer :: TIMER | e) m) =>
    Float -> GraphState m (Signal Time)

fps targetFrames =
    constant true >>= fpsWhen targetFrames


-- | Same as the `fps` function, but you can turn it on and off. Allows you
-- | to do brief animations based on user input without major inefficiencies.
-- | The first time delta after a pause is always zero, no matter how long
-- | the pause was. This way summing the deltas will actually give the amount
-- | of time that the output signal has been running.
fpsWhen ::
    forall e m. (MonadEff (ref :: REF, now :: NOW, delay :: DELAY, console :: CONSOLE, timer :: TIMER | e) m) =>
    Float -> Signal Bool -> GraphState m (Signal Time)

fpsWhen desiredFPS isOn = do
    -- TODO: This is not particularly elegant.
    start <- (toTime <<< unInstant) <$> liftEff now
    mbox <- mailbox 0.0
    changes <- dropRepeats isOn


    -- The initial state
    state <- liftEff $ newRef
        { lastTriggered: start
        , interval: Nothing :: Maybe IntervalId
        }

    let
        -- TODO: We could keep a fractional component and adjust ...
        msPerFrame =
            round $ 1000.0 / desiredFPS

        -- This will be called when an interval triggers
        onInterval = do
            currentTime <- (toTime <<< unInstant) <$> now 
            s <- readRef state
            send mbox.address (currentTime - s.lastTriggered)
            writeRef state $ s {lastTriggered = currentTime}

        -- This will be called once at startup, at then whenever isOn changes,
        -- with the value of isOn
        onChange = \on -> do
            currentTime <- (toTime <<< unInstant) <$> now
            s <- readRef state

            if on
                then do
                    -- We always start up again with 0.0
                    send mbox.address 0.0

                    i <- setInterval msPerFrame onInterval 
                    writeRef state
                        { lastTriggered : currentTime
                        , interval : Just i
                        }

                else do
                    case s.interval of
                        Just i ->
                            clearInterval i

                        Nothing ->
                            pure unit

                    writeRef state $ s {interval = Nothing}

    -- Hook up our handler
    handleChanges <- output "fpsWhenChanges" onChange changes 

    -- Kick off the first "change"
    liftEff $ current isOn >>= onChange

    -- And return the signal
    pure mbox.signal


-- | Takes a time interval `t`. The resulting signal is the current time, updated
-- | every `t`.
every ::
    forall e m. (MonadEff (ref :: REF, delay :: DELAY, now :: NOW, timer :: TIMER, console :: CONSOLE | e) m) =>
    Time -> GraphState m (Signal Time)

every t = do
    current <- (toTime <<< unInstant) <$> liftEff now
    mbox <- mailbox current

    liftEff $ void $ setInterval (round t) do
        future <- (toTime <<< unInstant) <$> now
        send mbox.address future

    pure mbox.signal


-- | Delay a signal by a certain amount of time. So `(delay second Mouse.clicks)`
-- | will update one second later than any mouse click.
delay :: 
    forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: NOW, console :: CONSOLE | e) m) =>
    Time -> Signal a -> GraphState m (Signal a)

delay period signal = do
    current <- liftEff $ current signal 
    mbox <- mailbox current

    let
        millis =
            round period

        react a =
            void $ setTimeout millis do
                send mbox.address a

    output "delay" react signal

    pure mbox.signal


-- | Takes a time `t` and any signal. The resulting boolean signal is true for
-- | time `t` after every event on the input signal. So ``(second `since`
-- | Mouse.clicks)`` would result in a signal that is true for one second after
-- | each mouse click and false otherwise.
since ::
    forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: NOW, console :: CONSOLE | e) m) =>
    Time -> Signal a -> GraphState m (Signal Bool)

since time signal = do
    start <- Signal.map (const 1) signal
    stop <- (delay time signal) >>= Signal.map (const (-1))
    delaydiff <- merge start stop >>= foldp (+) 0 
    Signal.map ((/=) 0) delaydiff
