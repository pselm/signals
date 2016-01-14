## Module Elm.Basics

#### `Float`

``` purescript
type Float = Number
```

#### `Bool`

``` purescript
type Bool = Boolean
```

#### `radians`

``` purescript
radians :: Float -> Float
```

Convert radians to standard Elm angles (radians). 

#### `degrees`

``` purescript
degrees :: Float -> Float
```

Convert degrees to standard Elm angles (radians). 

#### `turns`

``` purescript
turns :: Float -> Float
```

#### `fromPolar`

``` purescript
fromPolar :: { r :: Float, theta :: Float } -> { x :: Float, y :: Float }
```

#### `toPolar`

``` purescript
toPolar :: { x :: Float, y :: Float } -> { r :: Float, theta :: Float }
```

#### `(//)`

``` purescript
(//) :: forall a. (ModuloSemiring a) => a -> a -> a
```

_left-associative / precedence 7_

Integer division. The remainder is discarded. 

#### `rem`

``` purescript
rem :: forall a. (ModuloSemiring a) => a -> a -> a
```

#### `(%)`

``` purescript
(%) :: forall a. (Semiring a, Ring a, Ord a, ModuloSemiring a) => a -> a -> a
```

_left-associative / precedence 7_

#### `Pow`

``` purescript
class Pow a where
  pow :: a -> a -> a
```

##### Instances
``` purescript
Pow Int
Pow Number
```

#### `(^)`

``` purescript
(^) :: forall a. (Pow a) => a -> a -> a
```

_right-associative / precedence 8_

#### `abs`

``` purescript
abs :: forall a. (Ring a, Ord a) => a -> a
```

Take the absolute value of a number. 

#### `logBase`

``` purescript
logBase :: Float -> Float -> Float
```

#### `Order`

``` purescript
type Order = Ordering
```

#### `xor`

``` purescript
xor :: forall a. (BooleanAlgebra a) => a -> a -> a
```

The exclusive-or operator. `True` if exactly one input is `True`. 

#### `truncate`

``` purescript
truncate :: Float -> Int
```

Truncate a number, rounding towards zero. 

#### `ceiling`

``` purescript
ceiling :: Float -> Int
```

Ceiling function, rounding up. 

#### `toFloat`

``` purescript
toFloat :: Int -> Float
```

Convert an integer into a float. 

#### `isInfinite`

``` purescript
isInfinite :: Float -> Boolean
```

#### `toString`

``` purescript
toString :: forall a. (Show a) => a -> String
```

#### `(<<)`

``` purescript
(<<) :: forall a b c. (b -> c) -> (a -> b) -> a -> c
```

_right-associative / precedence 9_

#### `(>>)`

``` purescript
(>>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
```

_left-associative / precedence 9_

#### `(|>)`

``` purescript
(|>) :: forall a b. a -> (a -> b) -> b
```

_left-associative / precedence 0_

#### `(<|)`

``` purescript
(<|) :: forall a b. (a -> b) -> a -> b
```

_right-associative / precedence 0_

#### `identity`

``` purescript
identity :: forall a. a -> a
```

#### `always`

``` purescript
always :: forall a b. a -> b -> a
```


