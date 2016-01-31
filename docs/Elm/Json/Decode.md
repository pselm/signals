## Module Elm.Json.Decode

#### `Decoder`

``` purescript
newtype Decoder a
```

##### Instances
``` purescript
Functor Decoder
Alt Decoder
Apply Decoder
Applicative Decoder
Bind Decoder
Monad Decoder
```

#### `decodeString`

``` purescript
decodeString :: forall a. Decoder a -> String -> Result String a
```

#### `at`

``` purescript
at :: forall a. List String -> Decoder a -> Decoder a
```

#### `(:=)`

``` purescript
(:=) :: forall a. String -> Decoder a -> Decoder a
```

_left-associative / precedence -1_

#### `object1`

``` purescript
object1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

#### `object2`

``` purescript
object2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

#### `object3`

``` purescript
object3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

#### `object4`

``` purescript
object4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
```



#### `object5`

``` purescript
object5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
```



#### `object6`

``` purescript
object6 :: forall a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
```



#### `object7`

``` purescript
object7 :: forall a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
```



#### `object8`

``` purescript
object8 :: forall a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
```



#### `keyValuePairs`

``` purescript
keyValuePairs :: forall a. Decoder a -> Decoder (List (Tuple String a))
```

#### `dict`

``` purescript
dict :: forall a. Decoder a -> Decoder (Dict String a)
```

#### `oneOf`

``` purescript
oneOf :: forall a. List (Decoder a) -> Decoder a
```

#### `string`

``` purescript
string :: Decoder String
```

#### `float`

``` purescript
float :: Decoder Float
```

#### `int`

``` purescript
int :: Decoder Int
```

#### `bool`

``` purescript
bool :: Decoder Bool
```

#### `list`

``` purescript
list :: forall a. Decoder a -> Decoder (List a)
```

#### `array`

``` purescript
array :: forall a. Decoder a -> Decoder (Array a)
```

#### `null`

``` purescript
null :: forall a. a -> Decoder a
```

#### `maybe`

``` purescript
maybe :: forall a. Decoder a -> Decoder (Maybe a)
```

#### `value`

``` purescript
value :: Decoder Value
```

#### `decodeValue`

``` purescript
decodeValue :: forall a. Decoder a -> Value -> Result String a
```

#### `customDecoder`

``` purescript
customDecoder :: forall a b. Decoder a -> (a -> Result String b) -> Decoder b
```

#### `fail`

``` purescript
fail :: forall a. String -> Decoder a
```

#### `succeed`

``` purescript
succeed :: forall a. a -> Decoder a
```

#### `tuple1`

``` purescript
tuple1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

#### `tuple2`

``` purescript
tuple2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

#### `tuple3`

``` purescript
tuple3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

#### `tuple4`

``` purescript
tuple4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
```



#### `tuple5`

``` purescript
tuple5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
```



#### `tuple6`

``` purescript
tuple6 :: forall a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
```



#### `tuple7`

``` purescript
tuple7 :: forall a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
```



#### `tuple8`

``` purescript
tuple8 :: forall a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
```




