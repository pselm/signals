## Module Elm.Default

This module re-exports the things which Elm imports by default.

So, if you want the Elm default imports, you can do

`import Elm.Default`


### Re-exported from Data.Maybe:

#### `Maybe`

``` purescript
data Maybe a
  = Nothing
  | Just a
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.

##### Instances
``` purescript
Functor Maybe
Apply Maybe
Applicative Maybe
Alt Maybe
Plus Maybe
Alternative Maybe
Bind Maybe
Monad Maybe
MonadPlus Maybe
Extend Maybe
Invariant Maybe
(Semigroup a) => Semigroup (Maybe a)
(Semigroup a) => Monoid (Maybe a)
(Semiring a) => Semiring (Maybe a)
(ModuloSemiring a) => ModuloSemiring (Maybe a)
(Ring a) => Ring (Maybe a)
(DivisionRing a) => DivisionRing (Maybe a)
(Num a) => Num (Maybe a)
(Eq a) => Eq (Maybe a)
(Ord a) => Ord (Maybe a)
(Bounded a) => Bounded (Maybe a)
(BoundedOrd a) => BoundedOrd (Maybe a)
(BooleanAlgebra a) => BooleanAlgebra (Maybe a)
(Show a) => Show (Maybe a)
```

### Re-exported from Elm.Basics:

#### `Bool`

``` purescript
type Bool = Boolean
```

The Purescript equivalent of Elm's `Bool` is `Boolean`.

#### `Float`

``` purescript
type Float = Number
```

The Purescript equivalent of Elm's `Float` is `Number`.

#### `Order`

``` purescript
type Order = Ordering
```

Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.

Equivalent to Purescript's `Ordering`.

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

#### `abs`

``` purescript
abs :: forall a. (Ring a, Ord a) => a -> a
```

Take the absolute value of a number.

#### `acos`

``` purescript
acos :: Number -> Radians
```

Returns the inverse cosine of the argument.

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

#### `ceiling`

``` purescript
ceiling :: Float -> Int
```

Ceiling function, rounding up.

Equivalent to Purescript's `ceil`.

#### `clamp`

``` purescript
clamp :: forall a. (Ord a) => a -> a -> a -> a
```

Clamp a value between a minimum and a maximum. For example:

    let f = clamp 0 10
    f (-5) == 0
    f 5    == 5
    f 15   == 10


#### `compare`

``` purescript
compare :: forall a. (Ord a) => a -> a -> Ordering
```

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

#### `cos`

``` purescript
cos :: Radians -> Number
```

Returns the cosine of the argument.

#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```

Turn a function that expects a tuple into a function of two arguments.

#### `degrees`

``` purescript
degrees :: Float -> Float
```

Convert degrees to standard Elm angles (radians).

#### `e`

``` purescript
e :: Number
```

The base of natural logarithms, *e*, around 2.71828.

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `floor`

``` purescript
floor :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the closest integer equal to or
less than the argument. Values outside the `Int` range are clamped.

#### `fromPolar`

``` purescript
fromPolar :: { r :: Float, theta :: Float } -> { x :: Float, y :: Float }
```

Convert polar coordinates `{r, theta}` to Cartesian coordinates `{x, y}`.

Note that the Elm version uses tuples ... it seemed like this was a good
literals, Elm code using Tuples nees some modification in any event).

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).

#### `fst`

``` purescript
fst :: forall a b. Tuple a b -> a
```

Returns the first component of a tuple.

#### `identity`

``` purescript
identity :: forall a. a -> a
```

Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).

The Purescript equivalent is `id`.

#### `intDiv`

``` purescript
intDiv :: forall a. (ModuloSemiring a) => a -> a -> a
```

Integer division. The remainder is discarded.

In Purescript, you can simply use `/`.

#### `isInfinite`

``` purescript
isInfinite :: Float -> Bool
```

Determine whether a float is positive or negative infinity.

    isInfinite (0/0)     == False
    isInfinite (sqrt -1) == False
    isInfinite (1/0)     == True
    isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.

Note that this is not equivalent to the negation of Javascript's `isFinite()`.

#### `isNaN`

``` purescript
isNaN :: Number -> Boolean
```

Test whether a number is NaN

#### `logBase`

``` purescript
logBase :: Float -> Float -> Float
```

Calculate the logarithm of a number with a given base.

    logBase 10 100 == 2
    logBase 2 256 == 8

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

#### `mod`

``` purescript
mod :: forall a. (Ord a, ModuloSemiring a, Ring a) => a -> a -> a
```

Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).

     7 % 2 == 1
    -1 % 4 == 3

Note that this is not the same as Purescript's `Prelude.mod` --
for that, see `Basics.rem`.

#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `not`

``` purescript
not :: forall a. (BooleanAlgebra a) => a -> a
```

#### `pi`

``` purescript
pi :: Number
```

The ratio of the circumference of a circle to its diameter, around 3.14159.

#### `radians`

``` purescript
radians :: Float -> Float
```

Convert radians to standard Elm angles (radians).

#### `rem`

``` purescript
rem :: forall a. (ModuloSemiring a) => a -> a -> a
```

Find the remainder after dividing one number by another.

    7 `rem` 2 == 1
    -1 `rem` 4 == -1

Equivalent to Purescript's `Prelude.mod`.

#### `round`

``` purescript
round :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the nearest integer to the
argument. Values outside the `Int` range are clamped.

#### `sin`

``` purescript
sin :: Radians -> Number
```

Returns the sine of the argument.

#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```

Returns the second component of a tuple.

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

#### `toFloat`

``` purescript
toFloat :: Int -> Float
```

Convert an integer into a float.

Equivalent to Purescript's `toNumber`.

#### `toPolar`

``` purescript
toPolar :: { x :: Float, y :: Float } -> { r :: Float, theta :: Float }
```

Convert Cartesian coordinates `{x, y}` to polar coordinates `{r, theta}`.

Note that the Elm version uses tuples ... it seemed like this was a good
literals, Elm code using Tuples nees some modification in any event).

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).

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

#### `truncate`

``` purescript
truncate :: Float -> Int
```

Truncate a number, rounding towards zero.

#### `turns`

``` purescript
turns :: Float -> Float
```

Convert turns to standard Elm angles (radians).
One turn is equal to 360&deg;.

#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```

Turn a function of two arguments into a function that expects a tuple.

#### `xor`

``` purescript
xor :: forall a. (BooleanAlgebra a) => a -> a -> a
```

The exclusive-or operator. `True` if exactly one input is `True`.

#### `(%)`

``` purescript
infixl 7 mod as %
```

_left-associative / precedence 7_

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

#### `(//)`

``` purescript
infixl 7 intDiv as //
```

_left-associative / precedence 7_

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

#### `(<<)`

``` purescript
infixr 9 compose as <<
```

_right-associative / precedence 9_

#### `(<=)`

``` purescript
(<=) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _non-strictly less than_ another.

#### `(<|)`

``` purescript
infixr 0 applyFn as <|
```

_right-associative / precedence 0_

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

#### `(>>)`

``` purescript
infixl 9 composeFlipped as >>
```

_left-associative / precedence 9_

#### `(^)`

``` purescript
infixr 8 pow as ^
```

_right-associative / precedence 8_

#### `(|>)`

``` purescript
infixl 0 applyFnFlipped as |>
```

_left-associative / precedence 0_

#### `(||)`

``` purescript
(||) :: forall a. (BooleanAlgebra a) => a -> a -> a
```

_right-associative / precedence 2_

`(||)` is an alias for `disj`.

### Re-exported from Elm.Debug:

#### `crash`

``` purescript
crash :: forall a. String -> a
```

Crash the program with an error message.

#### `log`

``` purescript
log :: forall a. String -> a -> a
```

Log a tagged value on the developer console, and then return the value.

    1 + log "number" 1        -- equals 2, logs "number: 1"
    length (log "start" [])   -- equals 0, logs "start: []"

Notice that `log` is not a pure function! It should *only* be used for
investigating bugs or performance problems.

### Re-exported from Elm.List:

#### `List`

``` purescript
data List a
```

A strict linked list.

A list is either empty (represented by the `Nil` constructor) or non-empty, in
which case it consists of a head element, and another list (represented by the
`Cons` constructor).

##### Instances
``` purescript
(Show a) => Show (List a)
(Eq a) => Eq (List a)
(Ord a) => Ord (List a)
Semigroup (List a)
Monoid (List a)
Functor List
Foldable List
Unfoldable List
Traversable List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadPlus List
```

#### `(:)`

``` purescript
infixr 5 cons as :
```

_right-associative / precedence 5_

### Re-exported from Elm.Result:

#### `Result`

``` purescript
data Result error value
  = Ok value
  | Err error
```

A `Result` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.

##### Instances
``` purescript
Functor (Result a)
Bifunctor Result
Apply (Result e)
Applicative (Result e)
Alt (Result e)
Bind (Result e)
Monad (Result e)
Extend (Result e)
(Show a, Show b) => Show (Result a b)
(Eq a, Eq b) => Eq (Result a b)
(Ord a, Ord b) => Ord (Result a b)
(Bounded a, Bounded b) => Bounded (Result a b)
Foldable (Result a)
Bifoldable Result
Traversable (Result a)
Bitraversable Result
(Semiring b) => Semiring (Result a b)
(Semigroup b) => Semigroup (Result a b)
(Arbitrary a, Arbitrary b) => Arbitrary (Result a b)
(Coarbitrary a, Coarbitrary b) => Coarbitrary (Result a b)
```

### Re-exported from Elm.Signal:

#### `Signal`

``` purescript
newtype Signal a
```

A value that changes over time. So a `(Signal Int)` is an integer that is
varying as time passes, perhaps representing the current window width of the
browser. Every signal is updated at discrete moments in response to events in
the world.

