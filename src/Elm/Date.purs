
-- | Library for working with dates.
-- |
-- | Note that the Purescript `Month` and `DayOfWeek` types
-- | spell out the entire word for constructors ... e.g.
-- | `January` for `Jan` and `Monday` for `Mon`.
-- |
-- | Also, the Elm `Day` type is `DayOfWeek` in Purescript, and there are
-- | distinct types for `DayOfMonth` and `Year`.

module Elm.Date
    ( module Virtual
    , fromString, toTime, fromTime
    , year, month, day, dayOfWeek
    , hour, minute, second, millisecond
    ) where


-- For re-export

import Data.Date 
    ( Date, Year(..), Month(..), DayOfMonth, DayOfWeek(..)
    ) as Virtual

-- Internal

import Data.Date
    ( Date, DayOfWeek, DayOfMonth(..), Month, Year(..)
    , toEpochMilliseconds, fromEpochMilliseconds
    )

import Data.Time (SecondOfMinute(..), MinuteOfHour(..), MillisecondOfSecond(..), HourOfDay(..))

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Elm.Result (Result, fromMaybe)
import Elm.Time (Time)
import Prelude ((<<<), (++), show, ($))
import Data.Maybe (Maybe(..))


-- | Elm's `Day` type is equivalent to `DayOfWeek` in Purescript.
-- | 
-- | However, note that in Purescript the constructors spell out
-- | the name of the day ... i.e. `Saturday` instead of `Sat`.
type Day = DayOfWeek


-- | Attempt to read a date from a string.
fromString :: String -> Result String Date
fromString str =
    fromMaybe 
        ("unable to parse '" ++ str ++ "' as a date")
        (Data.Date.fromString str) 


-- | Convert a date into a time since midnight (UTC) of 1 January 1970 (i.e.
-- | [UNIX time](http://en.wikipedia.org/wiki/Unix_time)). Given the date 23 June
-- | 1990 at 11:45AM this returns the corresponding time.
toTime :: Date -> Time
toTime =
    Elm.Time.toTime <<< toEpochMilliseconds


-- | Take a UNIX time and convert it to a `Date`.
fromTime :: Time -> Date
fromTime time =
    case fromEpochMilliseconds (Elm.Time.fromTime time) of
         Just date -> date
         Nothing -> Elm.Debug.crash ("problem converting " ++ show time ++ " to Date")


-- | Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `1990`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
year :: Date -> Int
year d =
    case unsafePerformEff $ Data.Date.Locale.year d of
         Year n -> n


-- | Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the Month `June` as defined below.
-- |
-- | Note that in Purescript, the constructors for `Month` are fully spelled out,
-- | so it is 'June` instead of `Jun`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
month :: Date -> Month
month =
    unsafePerformEff <<< Data.Date.Locale.month


-- | Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `23`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
day :: Date -> Int
day d =
    case unsafePerformEff $ Data.Date.Locale.dayOfMonth d of
         DayOfMonth n -> n


-- | Extract the day of the week for a given date. Given the date 23 June
-- | 1990 at 11:45AM this returns the Day `Thursday` as defined below.
-- |
-- | Note that in Purescript, the days of the week are fully spelled out,
-- | so it is `Thursday` instead of `Thu`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
dayOfWeek :: Date -> DayOfWeek
dayOfWeek =
    unsafePerformEff <<< Data.Date.Locale.dayOfWeek


-- | Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `11`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
hour :: Date -> Int
hour d =
    case unsafePerformEff $ Data.Date.Locale.hourOfDay d of
        HourOfDay n -> n


-- | Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
-- \ this returns the integer `45`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
minute :: Date -> Int
minute d =
    case unsafePerformEff $ Data.Date.Locale.minuteOfHour d of
        MinuteOfHour n -> n


-- | Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `0`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
second :: Date -> Int
second d =
    case unsafePerformEff $ Data.Date.Locale.secondOfMinute d of
         SecondOfMinute n -> n


-- | Extract the millisecond of a given date. Given the date 23 June 1990 at 11:45:30.123AM
-- | this returns the integer `123`.
-- |
-- | As in the Elm implementation, this implicitly uses the current locale.
millisecond :: Date -> Int
millisecond d =
    case unsafePerformEff $ Data.Date.Locale.millisecondOfSecond d of
         MillisecondOfSecond n -> n

