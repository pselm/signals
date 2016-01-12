## Module Elm.Time

#### `Time`

``` purescript
type Time = Number
```

#### `toTime`

``` purescript
toTime :: forall a. (TimeValue a) => a -> Time
```

#### `fromTime`

``` purescript
fromTime :: forall a. (TimeValue a) => Time -> a
```

#### `millisecond`

``` purescript
millisecond :: Time
```

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




