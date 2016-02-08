## Module Elm.Int53

In Purescript, the `Int` type is restricted to 32-bit integers. However,
the underlying Javascript runtime is capable of working with 53-bit
integers. So, this module exposes an `Int53` type, for cases where you
want all 53 bits.

This is needed for some code ported from Elm, since the Elm `Int` type
can handle 53 bits.

#### `Int53`

``` purescript
newtype Int53
```

A JavaScript 53-bit signed integer.

##### Instances
``` purescript
Semiring Int53
Ring Int53
ModuloSemiring Int53
Eq Int53
Ord Int53
Bounded Int53
BoundedOrd Int53
Show Int53
Pow Int53
Arbitrary Int53
Int53Value Int53
```

#### `truncate`

``` purescript
truncate :: Number -> Int53
```

Convert a `Number` to an `Int53`, by rounding towards zero.
Values outside the `Int53` range are clamped.

#### `floor`

``` purescript
floor :: Number -> Int53
```

Convert a `Number` to an `Int53`, by taking the closest integer equal to or
less than the argument. Values outside the `Int53` range are clamped.

#### `ceil`

``` purescript
ceil :: Number -> Int53
```

Convert a `Number` to an `Int53`, by taking the closest integer equal to or
greater than the argument. Values outside the `Int53` range are clamped.

#### `round`

``` purescript
round :: Number -> Int53
```

Convert a `Number` to an `Int53`, by taking the nearest integer to the
argument. Values outside the `Int53` range are clamped.

#### `fromNumber`

``` purescript
fromNumber :: Number -> Maybe Int53
```

Creates an `Int53` from a `Number` value. The number must already be an
integer and fall within the valid range of values for the `Int53` type
otherwise `Nothing` is returned.

#### `toNumber`

``` purescript
toNumber :: Int53 -> Number
```

Converts an `Int53` value back into a `Number`. Any `Int53` is a valid `Number`
so there is no loss of precision with this function.

#### `fromString`

``` purescript
fromString :: String -> Maybe Int53
```

Reads an `Int53` from a `String` value. The number must parse as an integer
and fall within the valid range of values for the `Int53` type, otherwise
`Nothing` is returned.

#### `fromInt`

``` purescript
fromInt :: Int -> Int53
```

Converts an `Int` to an `Int53`.

#### `toInt`

``` purescript
toInt :: Int53 -> Int
```

Converts an `Int53` to an `Int`. Values outside the `Int` range are clamped.

#### `even`

``` purescript
even :: Int53 -> Boolean
```

Returns whether an `Int53` is an even number.

    even (fromInt 0) == true
    even (fromInt 1) == false

#### `odd`

``` purescript
odd :: Int53 -> Boolean
```

The negation of `even`.

    odd (fromInt 0) == false
    odd (fromInt 1) == true

#### `Int53Value`

``` purescript
class Int53Value a where
  toInt53 :: a -> Int53
  fromInt53 :: Int53 -> a
```

A class for cases where we'd like to accept eitner `Int` or `Int53`,
work with `Int53` internally, and then return whatever type we were
given. The conversions should do their best ... for instance, they
may clamp if necessary, or truncate etc.

##### Instances
``` purescript
Int53Value Int53
Int53Value Int
```


