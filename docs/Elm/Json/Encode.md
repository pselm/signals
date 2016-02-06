## Module Elm.Json.Encode

A library for turning Elm values into Json values.

This is mainly implemented via the `toForeign` method in
[purescript-foreign](https://pursuit.purescript.org/packages/purescript-foreign/0.7.2/docs/Data.Foreign#v:toForeign).

You could also consider the purescript-argonaut-* modules.

#### `Value`

``` purescript
type Value = Foreign
```

Represents a JavaScript value.

#### `encode`

``` purescript
encode :: Int -> Value -> String
```

Convert a `Value` into a prettified string. The first argument specifies
the amount of indentation in the resulting string.

    person =
        object
          [ Tuple "name" (string "Tom")
          , Tuple "age" (int 42)
          ]

    compact = encode 0 person
    -- {"name":"Tom","age":42}

    readable = encode 4 person
    -- {
    --     "name": "Tom",
    --     "age": 42
    -- }

#### `string`

``` purescript
string :: String -> Value
```

Turn a `String` into a `Value`.

#### `int`

``` purescript
int :: Int -> Value
```

Turn an `Int` into a `Value`.

#### `float`

``` purescript
float :: Float -> Value
```

Encode a Float. `Infinity` and `NaN` are encoded as `null`.

#### `bool`

``` purescript
bool :: Bool -> Value
```

Encode a `Bool`.

#### `null`

``` purescript
null :: Value
```

Encode a null value.

#### `object`

``` purescript
object :: List (Tuple String Value) -> Value
```

Encode a JSON object.

#### `psArray`

``` purescript
psArray :: Array Value -> Value
```

Encode Purescript's primitive `Array` type (distinct from Elm's `Array`).

#### `array`

``` purescript
array :: Array Value -> Value
```

Encode Elm's `Array` type.

#### `list`

``` purescript
list :: List Value -> Value
```

Encode a `List`.


