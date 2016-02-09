
-- | Library for working with time

module Elm.Time
    ( module Virtual
    , Time, toTime, fromTime
    , millisecond, second, minute, hour
    , inMilliseconds, inSeconds, inMinutes, inHours
    , every
    --, fps, fpsWhen, every
    --, timestamp, delay, since
    ) where


-- For re-export

import Data.Time
    ( class TimeValue
    ) as Virtual


-- Internal

import Data.Time
    ( Hours(..), Minutes(..), Seconds(..), Milliseconds(..)
    , class TimeValue, toMilliseconds, fromMilliseconds
    )

import Data.Int (round)
import Data.Date (Now, nowEpochMilliseconds)
import Prelude ((/), flip, id, ($), (<<<), bind, pure)
import Control.Monad.Eff.Timer (TIMER, interval)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Elm.Signal (Signal, send, mailbox, DELAY, GraphState)


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


inMilliseconds :: Time -> Number
inMilliseconds = id


inSeconds :: Time -> Number
inSeconds = divBy second


inMinutes :: Time -> Number
inMinutes = divBy minute


inHours :: Time -> Number
inHours = divBy hour


-- | Takes a time interval `t`. The resulting signal is the current time, updated
-- | every `t`.
every ::
    forall e m. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: TIMER, console :: CONSOLE | e) m) =>
    Time -> GraphState m (Signal Time)

every t = do
    now <- liftEff nowEpochMilliseconds
    mbox <- mailbox (toTime now)

    liftEff $ interval (round t) do
        future <- nowEpochMilliseconds
        send mbox.address (toTime future)

    pure mbox.signal

