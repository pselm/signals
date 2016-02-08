## Module Elm.Basics

The Elm equivalent of Purescript's `Prelude`.

Note that many of these functions are re-exported from other modules in
Purescript. The goal is to bundle together the same things that Elm's `Basics` module
bundles together.

In Elm, these functions are all imported by default. Purescript does not
have default imports, so you will need to import this manually. Alternatively,
you can import `Elm.Default`, which will import all of Elm's default imports.

#### `Float`

``` purescript
type Float = Number
```

The Purescript equivalent of Elm's `Float` is `Number`.

#### `Bool`

``` purescript
type Bool = Boolean
```

The Purescript equivalent of Elm's `Bool` is `Boolean`.

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

Convert turns to standard Elm angles (radians).
One turn is equal to 360&deg;.

#### `fromPolar`

``` purescript
fromPolar :: { r :: Float, theta :: Float } -> { x :: Float, y :: Float }
```

Convert polar coordinates `{r, theta}` to Cartesian coordinates `{x, y}`.

Note that the Elm version uses tuples ... it seemed like this was a good
literals, Elm code using Tuples nees some modification in any event).

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).

#### `toPolar`

``` purescript
toPolar :: { x :: Float, y :: Float } -> { r :: Float, theta :: Float }
```

Convert Cartesian coordinates `{x, y}` to polar coordinates `{r, theta}`.

Note that the Elm version uses tuples ... it seemed like this was a good
literals, Elm code using Tuples nees some modification in any event).

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).

#### `(//)`

``` purescript
infixl 7 intDiv as //
```

_left-associative / precedence 7_

#### `intDiv`

``` purescript
intDiv :: forall a. (ModuloSemiring a) => a -> a -> a
```

Integer division. The remainder is discarded.

In Purescript, you can simply use `/`.

#### `rem`

``` purescript
rem :: forall a. (ModuloSemiring a) => a -> a -> a
```

Find the remainder after dividing one number by another.

    7 `rem` 2 == 1
    -1 `rem` 4 == -1

Equivalent to Purescript's `Prelude.mod`.

#### `(%)`

``` purescript
infixl 7 mod as %
```

_left-associative / precedence 7_

#### `mod`

``` purescript
mod :: forall a. (Semiring a, Ring a, Ord a, ModuloSemiring a) => a -> a -> a
```

Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).

     7 % 2 == 1
    -1 % 4 == 3

Note that this is not the same as Purescript's `Prelude.mod` --
for that, see `Basics.rem`.

#### `Pow`

``` purescript
class Pow a where
  pow :: a -> a -> a
```

A class for things that can be raised to a power.

##### Instances
``` purescript
Pow Int
Pow Number
```

#### `(^)`

``` purescript
infixr 8 pow as ^
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

Calculate the logarithm of a number with a given base.

    logBase 10 100 == 2
    logBase 2 256 == 8

#### `Order`

``` purescript
type Order = Ordering
```

Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.

Equivalent to Purescript's `Ordering`.

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

Equivalent to Purescript's `ceil`.

#### `toFloat`

``` purescript
toFloat :: Int -> Float
```

Convert an integer into a float.

Equivalent to Purescript's `toNumber`.

#### `isInfinite`

``` purescript
isInfinite :: Float -> Boolean
```

Determine whether a float is positive or negative infinity.

    isInfinite (0/0)     == False
    isInfinite (sqrt -1) == False
    isInfinite (1/0)     == True
    isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.

Note that this is not equivalent to the negation of Javascript's `isFinite()`.

#### `toString`

``` purescript
toString :: forall a. (Show a) => a -> String
```

Turn any kind of value into a string. When you view the resulting string
with `Text.fromString` it should look just like the value it came from.

    toString 42 == "42"
    toString [1,2] == "[1,2]"
    toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""

Equivalent to Purescript's `show`.

#### `(<<)`

``` purescript
infixr 9 compose as <<
```

_right-associative / precedence 9_

#### `compose`

``` purescript
compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
```

Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    not << isEven << sqrt

You can think of this operator as equivalent to the following:

    (g << f)  ==  (\x -> g (f x))

So our example expands out to something like this:

    \n -> not (isEven (sqrt n))

Equivalent to Purescript's `<<<`.

#### `(>>)`

``` purescript
infixl 9 composeFlipped as >>
```

_left-associative / precedence 9_

#### `composeFlipped`

``` purescript
composeFlipped :: forall a b c. (a -> b) -> (b -> c) -> a -> c
```

Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    sqrt >> isEven >> not

This direction of function composition seems less pleasant than `(<<)` which
reads nicely in expressions like: `filter (not << isRegistered) students`

Equivalent to Purescript's `>>>`.

#### `(|>)`

``` purescript
infixl 0 applyFnFlipped as |>
```

_left-associative / precedence 0_

#### `applyFnFlipped`

``` purescript
applyFnFlipped :: forall a b. a -> (a -> b) -> b
```

Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
Consider the following code to create a pentagon:

    scale 2 (move (10,10) (filled blue (ngon 5 30)))

This can also be written as:

    ngon 5 30
      |> filled blue
      |> move (10,10)
      |> scale 2

Equivalent to Purescript's `#`.

#### `(<|)`

``` purescript
infixr 0 applyFn as <|
```

_right-associative / precedence 0_

#### `applyFn`

``` purescript
applyFn :: forall a b. (a -> b) -> a -> b
```

Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis. Consider the following code to create a text element:

    leftAligned (monospace (fromString "code"))

This can also be written as:

    leftAligned << monospace <| fromString "code"

Equivalent to Purescript's `$`.

#### `identity`

``` purescript
identity :: forall a. a -> a
```

Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).

The Purescript equivalent is `id`.

#### `always`

``` purescript
always :: forall a b. a -> b -> a
```

Create a [constant function](http://en.wikipedia.org/wiki/Constant_function),
a function that *always* returns the same value regardless of what input you give.
It is defined as:

    always a b = a

It totally ignores the second argument, so `always 42` is a function that always
returns 42. When you are dealing with higher-order functions, this comes in
handy more often than you might expect. For example, creating a zeroed out list
of length ten would be:

    map (always 0) [0..9]

The Purescript equivalent is `const`.


### Re-exported from Data.Int:

#### `floor`

``` purescript
floor :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the closest integer equal to or
less than the argument. Values outside the `Int` range are clamped.

#### `round`

``` purescript
round :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the nearest integer to the
argument. Values outside the `Int` range are clamped.

### Re-exported from Data.Ord:

#### `clamp`

``` purescript
clamp :: forall a. (Ord a) => a -> a -> a -> a
```

Clamp a value between a minimum and a maximum. For example:

    let f = clamp 0 10
    f (-5) == 0
    f 5    == 5
    f 15   == 10


#### `max`

``` purescript
max :: forall a. (Ord a) => a -> a -> a
```

Take the maximum of two values. If they compare to `EQ`, the first
argument is chosen.

#### `min`

``` purescript
min :: forall a. (Ord a) => a -> a -> a
```

Take the minimum of two values. If they compare to `EQ`, the first
argument is chosen.

### Re-exported from Data.Tuple:

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

##### Instances
``` purescript
(Show a, Show b) => Show (Tuple a b)
(Eq a, Eq b) => Eq (Tuple a b)
(Ord a, Ord b) => Ord (Tuple a b)
(Bounded a, Bounded b) => Bounded (Tuple a b)
(BoundedOrd a, BoundedOrd b) => BoundedOrd (Tuple a b)
Semigroupoid Tuple
(Semigroup a, Semigroup b) => Semigroup (Tuple a b)
(Monoid a, Monoid b) => Monoid (Tuple a b)
(Semiring a, Semiring b) => Semiring (Tuple a b)
(ModuloSemiring a, ModuloSemiring b) => ModuloSemiring (Tuple a b)
(Ring a, Ring b) => Ring (Tuple a b)
(DivisionRing a, DivisionRing b) => DivisionRing (Tuple a b)
(Num a, Num b) => Num (Tuple a b)
(BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)
Functor (Tuple a)
Invariant (Tuple a)
Bifunctor Tuple
(Semigroup a) => Apply (Tuple a)
Biapply Tuple
(Monoid a) => Applicative (Tuple a)
Biapplicative Tuple
(Semigroup a) => Bind (Tuple a)
(Monoid a) => Monad (Tuple a)
Extend (Tuple a)
Comonad (Tuple a)
(Lazy a, Lazy b) => Lazy (Tuple a b)
Foldable (Tuple a)
Bifoldable Tuple
Traversable (Tuple a)
Bitraversable Tuple
```

#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```

Turn a function that expects a tuple into a function of two arguments.

#### `fst`

``` purescript
fst :: forall a b. Tuple a b -> a
```

Returns the first component of a tuple.

#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```

Returns the second component of a tuple.

#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```

Turn a function of two arguments into a function that expects a tuple.

### Re-exported from Global:

#### `isNaN`

``` purescript
isNaN :: Number -> Boolean
```

Test whether a number is NaN

### Re-exported from Math:

#### `acos`

``` purescript
acos :: Number -> Radians
```

Returns the inverse cosine of the argument.

#### `asin`

``` purescript
asin :: Number -> Radians
```

Returns the inverse sine of the argument.

#### `atan`

``` purescript
atan :: Number -> Radians
```

Returns the inverse tangent of the argument.

#### `atan2`

``` purescript
atan2 :: Number -> Number -> Radians
```

Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
the inverse tangent of `y / x`, where the signs of both arguments are used
to determine the sign of the result.
If the first argument is negative, the result will be negative.
The result is the angle between the positive x axis and  a point `(x, y)`.

#### `cos`

``` purescript
cos :: Radians -> Number
```

Returns the cosine of the argument.

#### `e`

``` purescript
e :: Number
```

The base of natural logarithms, *e*, around 2.71828.

#### `pi`

``` purescript
pi :: Number
```

The ratio of the circumference of a circle to its diameter, around 3.14159.

#### `sin`

``` purescript
sin :: Radians -> Number
```

Returns the sine of the argument.

#### `sqrt`

``` purescript
sqrt :: Number -> Number
```

Returns the square root of the argument.

#### `tan`

``` purescript
tan :: Radians -> Number
```

Returns the tangent of the argument.

### Re-exported from Prelude:

#### `not`

``` purescript
not :: forall a. (BooleanAlgebra a) => a -> a
```

#### `compare`

``` purescript
compare :: forall a. (Ord a) => a -> a -> Ordering
```

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `(&&)`

``` purescript
(&&) :: forall a. (BooleanAlgebra a) => a -> a -> a
```

_right-associative / precedence 3_

`(&&)` is an alias for `conj`.

#### `(*)`

``` purescript
(*) :: forall a. (Semiring a) => a -> a -> a
```

_left-associative / precedence 7_

`(*)` is an alias for `mul`.

#### `(+)`

``` purescript
(+) :: forall a. (Semiring a) => a -> a -> a
```

_left-associative / precedence 6_

`(+)` is an alias for `add`.

#### `(++)`

``` purescript
(++) :: forall s. (Semigroup s) => s -> s -> s
```

_right-associative / precedence 5_

`(++)` is an alternative alias for `append`.

#### `(-)`

``` purescript
(-) :: forall a. (Ring a) => a -> a -> a
```

_left-associative / precedence 6_

`(-)` is an alias for `sub`.

#### `(/)`

``` purescript
(/) :: forall a. (ModuloSemiring a) => a -> a -> a
```

_left-associative / precedence 7_

`(/)` is an alias for `div`.

#### `(/=)`

``` purescript
(/=) :: forall a. (Eq a) => a -> a -> Boolean
```

_non-associative / precedence 4_

`(/=)` tests whether one value is _not equal_ to another. Shorthand for
`not (x == y)`.

#### `(<)`

``` purescript
(<) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _strictly less than_ another.

#### `(<=)`

``` purescript
(<=) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _non-strictly less than_ another.

#### `(==)`

``` purescript
(==) :: forall a. (Eq a) => a -> a -> Boolean
```

_non-associative / precedence 4_

`(==)` is an alias for `eq`. Tests whether one value is equal to another.

#### `(>)`

``` purescript
(>) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _strictly greater than_ another.

#### `(>=)`

``` purescript
(>=) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _non-strictly greater than_ another.

#### `(||)`

``` purescript
(||) :: forall a. (BooleanAlgebra a) => a -> a -> a
```

_right-associative / precedence 2_

`(||)` is an alias for `disj`.

