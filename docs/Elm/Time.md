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
inMilliseconds :: Time -> Float
```

#### `inSeconds`

``` purescript
inSeconds :: Time -> Float
```

#### `inMinutes`

``` purescript
inMinutes :: Time -> Float
```

#### `inHours`

``` purescript
inHours :: Time -> Float
```

#### `fps`

``` purescript
fps :: forall e m. (MonadEff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: TIMER | e) m) => Float -> GraphState m (Signal Time)
```

Takes desired number of frames per second (FPS). The resulting signal
gives a sequence of time deltas as quickly as possible until it reaches
the desired FPS. A time delta is the time between the last frame and the
current frame.

#### `fpsWhen`

``` purescript
fpsWhen :: forall e m. (MonadEff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE, timer :: TIMER | e) m) => Float -> Signal Bool -> GraphState m (Signal Time)
```

Same as the `fps` function, but you can turn it on and off. Allows you
to do brief animations based on user input without major inefficiencies.
The first time delta after a pause is always zero, no matter how long
the pause was. This way summing the deltas will actually give the amount
of time that the output signal has been running.

#### `every`

``` purescript
every :: forall e m. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: TIMER, console :: CONSOLE | e) m) => Time -> GraphState m (Signal Time)
```

Takes a time interval `t`. The resulting signal is the current time, updated
every `t`.

#### `delay`

``` purescript
delay :: forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: TIMER, console :: CONSOLE | e) m) => Time -> Signal a -> GraphState m (Signal a)
```

Delay a signal by a certain amount of time. So `(delay second Mouse.clicks)`
will update one second later than any mouse click.

#### `since`

``` purescript
since :: forall e m a. (MonadEff (ref :: REF, delay :: DELAY, now :: Now, timer :: TIMER, console :: CONSOLE | e) m) => Time -> Signal a -> GraphState m (Signal Bool)
```

Takes a time `t` and any signal. The resulting boolean signal is true for
time `t` after every event on the input signal. So ``(second `since`
Mouse.clicks)`` would result in a signal that is true for one second after
each mouse click and false otherwise.


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

### Re-exported from Elm.Signal:

#### `timestamp`

``` purescript
timestamp :: forall e m a. (MonadEff (ref :: REF | e) m) => Signal a -> GraphState m (Signal (Tuple Time a))
```

Add a timestamp to any signal. Timestamps increase monotonically. When you
create `(timestamp Mouse.x)`, an initial timestamp is produced. The timestamp
updates whenever `Mouse.x` updates.

Timestamp updates are tied to individual events, so `(timestamp Mouse.x)` and
`(timestamp Mouse.y)` will always have the same timestamp because they rely on
the same underlying event (`Mouse.position`).

