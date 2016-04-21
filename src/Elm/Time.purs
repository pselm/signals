
-- | Library for working with time

module Elm.Time
    ( module Virtual
    , Time, toTime, fromTime
    , millisecond, second, minute, hour
    , inMilliseconds, inSeconds, inMinutes, inHours
    , fps, fpsWhen, every
    , delay, since
    ) where


-- For re-export

import Data.Time (class TimeValue) as Virtual
import Elm.Signal (timestamp) as Virtual


-- Internal

import Data.Time
    ( Hours(..), Minutes(..), Seconds(..), Milliseconds(..)
    , class TimeValue, toMilliseconds, fromMilliseconds
    )

import Elm.Basics (Float, Bool)
import Data.Int (round)
import Elm.Maybe (Maybe(..))
import Data.Date (Now, nowEpochMilliseconds)
import Prelude ((/), flip, id, ($), (<<<), bind, pure, (-), unit, (>>=), void, const, negate, (+), (/=))
import DOM.Timer (Timer, Interval, interval, clearInterval, timeout)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Elm.Signal (Signal, dropRepeats, send, mailbox, constant, current, output, foldp, merge, DELAY, GraphState)


-- | Type alias to make it clearer when you are working with time values.
-- | Using the `Time` constants instead of raw numbers is very highly recommended.
-- |
-- | Note that Purescript's `Data.Time` class does something similar, but has more detailed
-- | time values, with separate types for `Hours`, `Minutes`, `Seconds` and `Milliseconds`.
type Time = Number 


-- | Convert any of Purescript's time values to `Time`.
toTime :: forall a. (TimeValue a) => a -> Time
toTime tv =
    case toMilliseconds tv of
         Milliseconds n -> n


-- | Convert from `Time` to any of Purescript's time values.
fromTime :: forall a. (TimeValue a) => Time -> a
fromTime =
    fromMilliseconds <<< Milliseconds


-- | Units of time, making it easier to specify things like a half-second
-- | `(500 * millisecond)` without remembering Elm&rsquo;s underlying units of time.
millisecond :: Time
millisecond =
    toTime $ Milliseconds 1.0


second :: Time 
second =
    toTime $ Seconds 1.0


minute :: Time
minute =
    toTime $ Minutes 1.0


hour :: Time 
hour =
    toTime $ Hours 1.0


divBy :: Number -> Number -> Number
divBy = flip (/)


inMilliseconds :: Time -> Float
inMilliseconds = id


inSeconds :: Time -> Float
inSeconds = divBy second


inMinutes :: Time -> Float
inMinutes = divBy minute


inHours :: Time -> Float
inHours = divBy hour


-- | Takes desired number of frames per second (FPS). The resulting signal
-- | gives a sequence of time deltas as quickly as possible until it reaches
-- | the desired FPS. A time delta is the time between the last frame and the
-- | current frame.
fps ::
    forall e m. (MonadEff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) m) =>
    Float -> GraphState m (Signal Time)

fps targetFrames =
    constant true >>= fpsWhen targetFrames


-- | Same as the `fps` function, but you can turn it on and off. Allows you
-- | to do brief animations based on user input without major inefficiencies.
-- | The first time delta after a pause is always zero, no matter how long
-- | the pause was. This way summing the deltas will actually give the amount
-- | of time that the output signal has been running.
fpsWhen ::
    forall e m. (MonadEff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: Timer | e) m) =>
    Float -> Signal Bool -> GraphState m (Signal Time)

fpsWhen desiredFPS isOn = do
    -- TODO: This is not particularly elegant.
    now <- liftEff nowEpochMilliseconds
    mbox <- mailbox 0.0
    changes <- dropRepeats isOn


    -- The initial state
    state <- liftEff $ newRef
        { lastTriggered: now
        , interval: Nothing :: Maybe Interval
        }

    let
        -- TODO: We could keep a fractional component and adjust ...
        msPerFrame =
            round $ 1000.0 / desiredFPS

        -- This will be called when an interval triggers
        onInterval = do
            currentTime <- nowEpochMilliseconds
            s <- readRef state
            send mbox.address $ toTime (currentTime - s.lastTriggered)
            writeRef state $ s {lastTriggered = currentTime}

        -- This will be called once at startup, at then whenever isOn changes,
        -- with the value of isOn
        onChange = \on -> do
            currentTime <- nowEpochMilliseconds
            s <- readRef state

            if on
                then do
                    -- We always start up again with 0.0
                    send mbox.address 0.0

                    i <- interval msPerFrame onInterval 
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
    forall e m. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: Timer, console :: CONSOLE | e) m) =>
    Time -> GraphState m (Signal Time)

every t = do
    now <- liftEff nowEpochMilliseconds
    mbox <- mailbox (toTime now)

    liftEff $ interval (round t) do
        future <- nowEpochMilliseconds
        send mbox.address (toTime future)

    pure mbox.signal


-- | Delay a signal by a certain amount of time. So `(delay second Mouse.clicks)`
-- | will update one second later than any mouse click.
delay :: 
    forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: Timer, console :: CONSOLE | e) m) =>
    Time -> Signal a -> GraphState m (Signal a)

delay period signal = do
    current <- liftEff $ current signal 
    mbox <- mailbox current

    let
        millis =
            round period

        react a =
            void $ timeout millis do
                send mbox.address a

    output "delay" react signal

    pure mbox.signal


-- | Takes a time `t` and any signal. The resulting boolean signal is true for
-- | time `t` after every event on the input signal. So ``(second `since`
-- | Mouse.clicks)`` would result in a signal that is true for one second after
-- | each mouse click and false otherwise.
since ::
    forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: Timer, console :: CONSOLE | e) m) =>
    Time -> Signal a -> GraphState m (Signal Bool)

since time signal = do
    start <- Elm.Signal.map (const 1) signal
    stop <- (delay time signal) >>= Elm.Signal.map (const (-1))
    delaydiff <- merge start stop >>= foldp (+) 0 
    Elm.Signal.map ((/=) 0) delaydiff
