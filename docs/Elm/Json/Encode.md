## Module Elm.Json.Encode

#### `Value`

``` purescript
type Value = Foreign
```

Represents a JavaScript value. 

#### `encode`

``` purescript
encode :: Int -> Value -> String
```

#### `string`

``` purescript
string :: String -> Value
```



#### `int`

``` purescript
int :: Int -> Value
```



#### `float`

``` purescript
float :: Float -> Value
```

Encode a Float. `Infinity` and `NaN` are encoded as `null`.

#### `bool`

``` purescript
bool :: Boolean -> Value
```



#### `null`

``` purescript
null :: Value
```



#### `object`

``` purescript
object :: List (Tuple String Value) -> Value
```



#### `jsArray`

``` purescript
jsArray :: Array Value -> Value
```

#### `array`

``` purescript
array :: Array Value -> Value
```



#### `list`

``` purescript
list :: List Value -> Value
```




