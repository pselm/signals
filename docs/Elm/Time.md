## Module Elm.Time

Library for working with time

#### `Time`

``` purescript
type Time = Number
```

Type alias to make it clearer when you are working with time values.
Using the `Time` constants instead of raw numbers is very highly recommended.

Note that Purescript's `Data.Time` class does something similar, but has more detailed
time values, with separate types for `Hours`, `Minutes`, `Seconds` and `Milliseconds`.

#### `toTime`

``` purescript
toTime :: forall a. (TimeValue a) => a -> Time
```

Convert any of Purescript's time values to `Time`.

#### `fromTime`

``` purescript
fromTime :: forall a. (TimeValue a) => Time -> a
```

Convert from `Time` to any of Purescript's time values.

#### `millisecond`

``` purescript
millisecond :: Time
```

Units of time, making it easier to specify things like a half-second
`(500 * millisecond)` without remembering Elm&rsquo;s underlying units of time.

#### `second`

``` purescript
second :: Time
```

#### `minute`

``` purescript
minute :: Time
```

#### `hour`

``` purescript
hour :: Time
```

#### `inMilliseconds`

``` purescript
inMilliseconds :: Time -> Number
```

#### `inSeconds`

``` purescript
inSeconds :: Time -> Number
```

#### `inMinutes`

``` purescript
inMinutes :: Time -> Number
```

#### `inHours`

``` purescript
inHours :: Time -> Number
```


### Re-exported from Data.Time:

#### `TimeValue`

``` purescript
class TimeValue a
```

##### Instances
``` purescript
TimeValue Hours
TimeValue Minutes
TimeValue Seconds
TimeValue Milliseconds
```

