module Elm.Time
    ( module Virtual
    , Time(), toTime, fromTime
    , millisecond, second, minute, hour
    , inMilliseconds, inSeconds, inMinutes, inHours
    --, fps, fpsWhen, every
    --, timestamp, delay, since
    ) where


-- For re-export

import Data.Time
    ( TimeValue
    ) as Virtual


-- Internal

import Data.Time
    ( Hours(..), Minutes(..), Seconds(..), Milliseconds(..)
    , TimeValue, toMilliseconds, fromMilliseconds
    )

import Prelude ((/), flip, id, ($), (<<<))


{-| Type alias to make it clearer when you are working with time values.
Using the `Time` constants instead of raw numbers is very highly recommended.
-}
type Time = Number 


toTime :: forall a. (TimeValue a) => a -> Time
toTime tv =
    case toMilliseconds tv of
         Milliseconds n -> n


fromTime :: forall a. (TimeValue a) => Time -> a
fromTime =
    fromMilliseconds <<< Milliseconds


{-| Units of time, making it easier to specify things like a half-second
`(500 * millisecond)` without remembering Elm&rsquo;s underlying units of time.
-}
millisecond :: Time
millisecond =
    toTime $ Milliseconds 1.0


{-|-}
second :: Time 
second =
    toTime $ Seconds 1.0


{-|-}
minute :: Time
minute =
    toTime $ Minutes 1.0


{-|-}
hour :: Time 
hour =
    toTime $ Hours 1.0


divBy :: Number -> Number -> Number
divBy = flip (/)


{-|-}
inMilliseconds :: Time -> Number
inMilliseconds = id


{-|-}
inSeconds :: Time -> Number
inSeconds = divBy second


{-|-}
inMinutes :: Time -> Number
inMinutes = divBy minute


{-|-}
inHours :: Time -> Number
inHours = divBy hour


-- TODO: Add Signal-related stuff 
