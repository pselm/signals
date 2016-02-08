## Module Elm.Date

Library for working with dates.

Note that the Purescript `Month` and `DayOfWeek` types
spell out the entire word for constructors ... e.g.
`January` for `Jan` and `Monday` for `Mon`.

Also, the Elm `Day` type is `DayOfWeek` in Purescript, and there are
distinct types for `DayOfMonth` and `Year`.

#### `fromString`

``` purescript
fromString :: String -> Result String Date
```

Attempt to read a date from a string.

#### `toTime`

``` purescript
toTime :: Date -> Time
```

Convert a date into a time since midnight (UTC) of 1 January 1970 (i.e.
[UNIX time](http://en.wikipedia.org/wiki/Unix_time)). Given the date 23 June
1990 at 11:45AM this returns the corresponding time.

#### `fromTime`

``` purescript
fromTime :: Time -> Date
```

Take a UNIX time and convert it to a `Date`.

#### `year`

``` purescript
year :: Date -> Int
```

Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `1990`.

As in the Elm implementation, this implicitly uses the current locale.

#### `month`

``` purescript
month :: Date -> Month
```

Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
this returns the Month `June` as defined below.

Note that in Purescript, the constructors for `Month` are fully spelled out,
so it is 'June` instead of `Jun`.

As in the Elm implementation, this implicitly uses the current locale.

#### `day`

``` purescript
day :: Date -> Int
```

Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `23`.

As in the Elm implementation, this implicitly uses the current locale.

#### `dayOfWeek`

``` purescript
dayOfWeek :: Date -> DayOfWeek
```

Extract the day of the week for a given date. Given the date 23 June
1990 at 11:45AM this returns the Day `Thursday` as defined below.

Note that in Purescript, the days of the week are fully spelled out,
so it is `Thursday` instead of `Thu`.

As in the Elm implementation, this implicitly uses the current locale.

#### `hour`

``` purescript
hour :: Date -> Int
```

Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `11`.

As in the Elm implementation, this implicitly uses the current locale.

#### `minute`

``` purescript
minute :: Date -> Int
```

Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM

As in the Elm implementation, this implicitly uses the current locale.

#### `second`

``` purescript
second :: Date -> Int
```

Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `0`.

As in the Elm implementation, this implicitly uses the current locale.

#### `millisecond`

``` purescript
millisecond :: Date -> Int
```

Extract the millisecond of a given date. Given the date 23 June 1990 at 11:45:30.123AM
this returns the integer `123`.

As in the Elm implementation, this implicitly uses the current locale.


### Re-exported from Data.Date:

#### `Date`

``` purescript
newtype Date
```

A combined date/time value. `Date`s cannot be constructed directly to
ensure they are not the `Invalid Date` value, and instead must be created
via `fromJSDate`, `fromEpochMilliseconds`, `fromString`, etc. or the `date`
and `dateTime` functions in the `Data.Date.Locale` and `Data.Date.UTC`
modules.

##### Instances
``` purescript
Eq Date
Ord Date
Show Date
```

#### `DayOfMonth`

``` purescript
newtype DayOfMonth
```

A day-of-month date component value.

##### Instances
``` purescript
Eq DayOfMonth
Ord DayOfMonth
```

#### `DayOfWeek`

``` purescript
data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
```

A day-of-week date component value.

##### Instances
``` purescript
Eq DayOfWeek
Ord DayOfWeek
Bounded DayOfWeek
BoundedOrd DayOfWeek
Show DayOfWeek
Enum DayOfWeek
```

#### `Month`

``` purescript
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
```

A month date component value.

##### Instances
``` purescript
Eq Month
Ord Month
Bounded Month
BoundedOrd Month
Show Month
Enum Month
```

#### `Year`

``` purescript
newtype Year
  = Year Int
```

A year date component value.

##### Instances
``` purescript
Eq Year
Ord Year
Semiring Year
Ring Year
Show Year
```

