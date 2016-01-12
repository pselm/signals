## Module Elm.Date

#### `fromString`

``` purescript
fromString :: String -> Result String Date
```

Attempt to read a date from a string. 

#### `toTime`

``` purescript
toTime :: Date -> Time
```

#### `fromTime`

``` purescript
fromTime :: Time -> Date
```

Take a UNIX time and convert it to a `Date`. 

#### `year`

``` purescript
year :: Date -> Int
```

#### `month`

``` purescript
month :: Date -> Month
```

#### `day`

``` purescript
day :: Date -> Int
```

#### `dayOfWeek`

``` purescript
dayOfWeek :: Date -> DayOfWeek
```

#### `hour`

``` purescript
hour :: Date -> Int
```

#### `minute`

``` purescript
minute :: Date -> Int
```

#### `second`

``` purescript
second :: Date -> Int
```

#### `millisecond`

``` purescript
millisecond :: Date -> Int
```


