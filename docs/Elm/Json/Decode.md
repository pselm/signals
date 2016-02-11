## Module Elm.Json.Decode

A way to turn Json values into Elm values. A `Decoder a` represents a
decoding operation that will either produce a value of type `a`, or fail.

Elm's `Json.Decode` doesn't seem to be quite like any existing Purescript
package, so I've re-implemented it, using parts of purescript-foreign as
a base. For other approaches to decoding JSON in Purescript, you could see
purescript-foreign, and the purescript-argonaut-* packages.

#### `Decoder`

``` purescript
newtype Decoder a
```

Represents a way of decoding JSON values. If you have a `(Decoder (List String))`
it will attempt to take some JSON value and turn it into a list of strings.
These decoders are easy to put together so you can create more and more complex
decoders.

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

Using a certain decoder, attempt to parse a JSON string. If the decoder
fails, you will get a string message telling you why.

    decodeString (tuple2 (,) float float) "[3,4]"                  -- Ok (3,4)
    decodeString (tuple2 (,) float float) "{ \"x\": 3, \"y\": 4 }" -- Err ""

#### `at`

``` purescript
at :: forall f a. (Foldable f) => f String -> Decoder a -> Decoder a
```

Access a nested field, making it easy to dive into big structures. This is
really a helper function so you do not need to write `(:=)` so many times.

    -- object.target.value = 'hello'
    value : Decoder String
    value =
        at ["target", "value"] string

It is defined as

    at fields decoder =
        List.foldr (:=) decoder fields

Note that the signature is defined in terms of `Foldable` so that it will
work with `Array` or `List` (among others).

#### `field`

``` purescript
field :: forall a. String -> Decoder a -> Decoder a
```

Applies the decoder to the field with the given name.
Fails if the JSON object has no such field.

    nameAndAge : Decoder (String,Int)
    nameAndAge =
        object2 (,)
          ("name" := string)
          ("age" := int)

    optionalProfession : Decoder (Maybe String)
    optionalProfession =
        maybe ("profession" := string)

#### `(:=)`

``` purescript
infixl 4 field as :=
```

_left-associative / precedence 4_

#### `object1`

``` purescript
object1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

Apply a function to a decoder. You can use this function as `map` if you
must (which can be done with any `objectN` function actually).

    object1 sqrt ("x" := float)

#### `object2`

``` purescript
object2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

Use two different decoders on a JS value. This is nice for extracting
multiple fields from an object.

    point : Decoder (Float,Float)
    point =
        object2 (,)
          ("x" := float)
          ("y" := float)

#### `object3`

``` purescript
object3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

Use three different decoders on a JS value. This is nice for extracting
multiple fields from an object.

    type alias Job = { name : String, id : Int, completed : Bool }

    point : Decoder Job
    point =
        object3 Job
          ("name" := string)
          ("id" := int)
          ("completed" := bool)

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
keyValuePairs :: forall f a. (Monoid (f (Tuple String a)), Applicative f) => Decoder a -> Decoder (f (Tuple String a))
```

Turn any object into a list of key-value pairs, including inherited enumerable properties. Fails if _any_ value can't be
decoded with the given decoder.

    -- { "tom": 89, "sue": 92, "bill": 97, ... }
    grades : Decoder (List (String, Int))
    grades =
        keyValuePairs int

The container for the return type is polymorphic in order to accommodate `List` or `Array`, among others.

#### `dict`

``` purescript
dict :: forall a. Decoder a -> Decoder (Dict String a)
```

Turn any object into a dictionary of key-value pairs, including inherited enumerable properties. Fails if _any_ value can't be
decoded with the given decoder.

    -- { "mercury": 0.33, "venus": 4.87, "earth": 5.97, ... }
    planetMasses : Decoder (Dict String Float)
    planetMasses =
        dict float

#### `oneOf`

``` purescript
oneOf :: forall f a. (Foldable f) => f (Decoder a) -> Decoder a
```

Try out multiple different decoders. This is helpful when you are dealing
with something with a very strange shape and when `andThen` does not help
narrow things down so you can be more targeted.

    -- [ [3,4], { "x":0, "y":0 }, [5,12] ]

    points : Decoder (List (Float,Float))
    points =
        list point

    point : Decoder (Float,Float)
    point =
        oneOf
        [ tuple2 (,) float float
        , object2 (,) ("x" := float) ("y" := float)
        ]

The container has a polymorphic type to accommodate `List` or `Array`,
among others.

#### `extractForeign`

``` purescript
extractForeign :: forall a. (IsForeign a) => Decoder a
```

Extract any value with an `IsForeign` instance.

Note that this is not in the Elm API.

#### `string`

``` purescript
string :: Decoder String
```

Extract a string.

    -- ["John","Doe"]

    name : Decoder (String, String)
    name =
        tuple2 (,) string string

#### `float`

``` purescript
float :: Decoder Float
```

Extract a float.

    -- [ 6.022, 3.1415, 1.618 ]

    numbers : Decoder (List Float)
    numbers =
        list float

#### `int`

``` purescript
int :: Decoder Int
```

Extract an integer.

    -- { ... "age": 42 ... }

    age : Decoder Int
    age =
        "age" := int

#### `bool`

``` purescript
bool :: Decoder Bool
```

Extract a boolean.

    -- { ... "checked": true ... }

    checked : Decoder Bool
    checked =
        "checked" := bool

#### `list`

``` purescript
list :: forall a. Decoder a -> Decoder (List a)
```

Extract a List from a JS array.

    -- [1,2,3,4]

    numbers : Decoder (List Int)
    numbers =
        list int

#### `array`

``` purescript
array :: forall f a. (Unfoldable f) => Decoder a -> Decoder (f a)
```

Extract an Array from a JS array.

    -- [1,2,3,4]

    numbers : Decoder (Array Int)
    numbers =
        array int

The return type is polymorphic to accommodate `Array` and `Elm.Array`,
among others.

#### `unfoldable`

``` purescript
unfoldable :: forall f a. (Unfoldable f) => Decoder a -> Decoder (f a)
```

Extract any `Unfoldable` from a JS array.

    -- [1,2,3,4]

    numbers : Decoder (Array Int)
    numbers =
        unfoldable int

Note that this is not part of the Elm API.

#### `null`

``` purescript
null :: forall a. a -> Decoder a
```

Decode null as the value given, and fail otherwise. Primarily useful for
creating *other* decoders.

    numbers : Decoder [Int]
    numbers =
        list (oneOf [ int, null 0 ])

This decoder treats `null` as `Nothing`, and otherwise tries to produce a
`Just`.

    nullOr : Decoder a -> Decoder (Maybe a)
    nullOr decoder =
        oneOf
        [ null Nothing
        , map Just decoder
        ]

#### `maybe`

``` purescript
maybe :: forall a. Decoder a -> Decoder (Maybe a)
```

Extract a Maybe value, wrapping successes with `Just` and turning any
failure in `Nothing`. If you are expecting that a field can sometimes be `null`,
it's better to check for it [explicitly](#null), as this function will swallow
errors from ill-formed JSON.

The following code decodes JSON objects that may not have a profession field.

    -- profession: Just "plumber"
    -- { name: "Tom", age: 31, profession: "plumber" }
    -- profession: Nothing
    -- { name: "Sue", age: 42 }
    -- { name: "Amy", age: 27, profession: null }
    -- { name: "Joe", age: 36, profession: ["something", "unexpected"] }

    type alias Person =
        { name : String
        , age : Int
        , profession : Maybe String
        }

    person : Decoder Person
    person =
        object3 Person
          ("name" := string)
          ("age" := int)
          (maybe ("profession" := string))

#### `value`

``` purescript
value :: Decoder Value
```

Bring in an arbitrary JSON value. Useful if you need to work with crazily
formatted data. For example, this lets you create a parser for "variadic" lists
where the first few types are different, followed by 0 or more of the same
type.

    variadic2 : (a -> b -> List c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
    variadic2 f a b c =
        let
            combineResults = List.foldr (Result.map2 (::)) (Ok [])
        in
            customDecoder (list value) (\jsonList ->
                case jsonList of
                  one :: two :: rest ->
                      Result.map3 f
                        (decodeValue a one)
                        (decodeValue b two)
                        (combineResults (List.map (decodeValue c) rest))

                  _ -> Result.Err "expecting at least two elements in the array")

#### `decodeValue`

``` purescript
decodeValue :: forall a. Decoder a -> Value -> Result String a
```

Using a certain decoder, attempt to parse a raw `Json.Value`. You can pass
a `Json.Value` into Elm through a port, so this can let you handle data with
extra weird shapes or stuff that currently is not allowed through ports
automatically.

    port jsonValues : Signal Json.Value

    shapes : Signal (Result String Shape)
    shapes =
      Signal.map (decodeValue shape) jsonValues

    type Shape
        = Rectangle Float Float
        | Circle Float

    shape : Decoder Shape  -- see definition in `andThen` docs

#### `customDecoder`

``` purescript
customDecoder :: forall a b. Decoder a -> (a -> Result String b) -> Decoder b
```

Create a custom decoder that may do some fancy computation. See the `value`
documentation for an example usage.

#### `fail`

``` purescript
fail :: forall a. String -> Decoder a
```

A decoder that always fails. Useful when paired with `andThen` or `oneOf`
to improve error messages when things go wrong. For example, the following
decoder is able to provide a much more specific error message when `fail` is
the last option.

    point : Decoder (Float,Float)
    point =
        oneOf
        [ tuple2 (,) float float
        , object2 (,) ("x" := float) ("y" := float)
        , fail "expecting some kind of point"
        ]

#### `succeed`

``` purescript
succeed :: forall a. a -> Decoder a
```

A decoder that always succeeds. Useful when paired with `andThen` or
`oneOf` but everything is supposed to work out at the end. For example,
maybe you have an optional field that can have a default value when it is
missing.

    -- { x:3, y:4 } or { x:3, y:4, z:5 }

    point3D : Decoder (Float,Float,Float)
    point3D =
        object3 (,,)
          ("x" := float)
          ("y" := float)
          (oneOf [ "z" := float, succeed 0 ])

#### `tuple1`

``` purescript
tuple1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

Handle an array with exactly one element.

    extractString : Decoder String
    extractString =
        tuple1 identity string

    authorship : Decoder String
    authorship =
        oneOf
          [ tuple1 (\author -> "Author: " ++ author) string
          , list string |> map (\authors -> "Co-authors: " ++ String.join ", " authors)
          ]

#### `tuple2`

``` purescript
tuple2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

Handle an array with exactly two elements. Useful for points and simple
pairs.

    -- [3,4] or [0,0]
    point : Decoder (Float,Float)
    point =
        tuple2 (,) float float

    -- ["John","Doe"] or ["Hermann","Hesse"]
    name : Decoder Name
    name =
        tuple2 Name string string

    type alias Name = { first : String, last : String }

#### `tuple3`

``` purescript
tuple3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

Handle an array with exactly three elements.

    -- [3,4,5] or [0,0,0]
    point3D : Decoder (Float,Float,Float)
    point3D =
        tuple3 (,,) float float float


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




### Re-exported from Elm.Apply:

#### `andMap`

``` purescript
andMap :: forall a b f. (Apply f) => f (a -> b) -> f a -> f b
```

Map a function in a container to a value in a container.

This is the equivalent of Purescript's `apply`.

#### `map2`

``` purescript
map2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `map3`

``` purescript
map3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map4`

``` purescript
map4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map5`

``` purescript
map5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
```

Given some computation, chain its result with another computation.

`andThen` is equivalent to Purescript's `bind`.

### Re-exported from Elm.Json.Encode:

#### `Value`

``` purescript
type Value = Foreign
```

Represents a JavaScript value.

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. (Functor f) => (a -> b) -> f a -> f b
```

