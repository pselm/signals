module Elm.Basics 
    ( module Virtual
    , Order(..)
    , Pow, pow, (^)
    , abs, xor, (//), rem, (%)
    , Float(), logBase, truncate, ceiling, toFloat
    , radians, degrees, turns, fromPolar, toPolar
    , toString, isInfinite
    , identity, always, (<|), (|>), (<<), (>>)
    , Bool()
    ) where


-- For re-export
import Prelude
    ( (*), (+), (-), (/), negate
    , compare
    , (==), (/=), (<), (>), (<=), (>=)
    , (&&), (||), not
    , (++)
    , flip
    ) as Virtual
    
import Data.Ord (max, min, clamp) as Virtual
import Global (isNaN) as Virtual
import Data.Tuple (Tuple(..), fst, snd, curry, uncurry) as Virtual
import Math (sqrt, e, pi, sin, cos, tan, acos, asin, atan, atan2) as Virtual
import Data.Int (round, floor) as Virtual


-- For internal use
import Prelude
    ( id, (<<<), (>>>), ($), (#), const
    , (*), (/), (+)
    , (>), (>=), (==)
    , (||), (&&)
    , Ring, negate, not, zero
    , Show, show
    , Ord, Ordering(..)
    )

import Math (cos, sqrt, log, pi, sin, atan2)
import Data.Int (round, toNumber)


type Float = Number
type Bool = Boolean


{-| Convert radians to standard Elm angles (radians). -}
radians :: Float -> Float
radians = id


{-| Convert degrees to standard Elm angles (radians). -}
degrees :: Float -> Float
degrees d =
    d * pi / 180.0


{-| Convert turns to standard Elm angles (radians).
One turn is equal to 360&deg;.
-}
turns :: Float -> Float
turns t =
    2.0 * pi * t


{-| Convert polar coordinates {r, theta} to Cartesian coordinates {x, y}.

Note that the Elm version uses tuples ... it seemed like this was a good
candidate for records instead.

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).
-}
fromPolar :: {r :: Float, theta :: Float} -> {x :: Float, y :: Float}
fromPolar {r, theta} =
    { x: r * cos theta
    , y: r * sin theta
    }


{-| Convert Cartesian coordinates {x, y} to polar coordinates {r, theta}.

Note that the Elm version uses tuples ... it seemed like this was a good
candidate for records instead.

If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).
-}
toPolar :: {x :: Float, y :: Float} -> {r :: Float, theta :: Float}
toPolar {x, y} =
    { r: sqrt (x * x + y * y)
    , theta: atan2 y x
    }


infixl 7 //

{-| Integer division. The remainder is discarded. -}
(//) :: Int -> Int -> Int
(//) = Prelude.(/)


-- I'd like to do the following, but it looks like it doesn't work.
--
-- infixl 7 `rem`

{-| Find the remainder after dividing one number by another.

     7 `rem` 2 == 1
    -1 `rem` 4 == -1

Note that this is the Purescript `Prelude.mod`.
-}
rem :: Int -> Int -> Int
rem = Prelude.mod


infixl 7 %

{-| Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).

     7 % 2 == 1
    -1 % 4 == 3

Note that this represents different semantics than the Purescript `Prelude.(%)`.
-}
(%) :: Int -> Int -> Int
(%) a b =
    let
        r =
            a `rem` b

        m =
            if a == 0
                then
                    0 
                else 
                    if b > 0
                        then
                            if a >= 0
                                then r 
                                else r + b
                        else
                            -(-a % -b)

    in
        if m == b
           then 0
           else m


class Pow a b where
    pow :: a -> b -> a

instance intIntPow :: Pow Int Int where
    pow a b = round <| Math.pow (toNumber a) (toNumber b)

instance floatFloatPow :: Pow Number Number where
    pow a b = Math.pow a b

instance intFloatPow :: Pow Int Number where
    pow a b = round <| Math.pow (toNumber a) b

instance floatIntPow :: Pow Number Int where
    pow a b = Math.pow a (toNumber b)

infixr 8 ^

{-| Exponentiation

    3^2 == 9
-}
(^) :: forall a b. (Pow a b) => a -> b -> a 
(^) = pow


{-| Take the absolute value of a number. -}
abs :: forall a. (Ring a, Ord a) => a -> a
abs a =
    if a >= zero
        then a
        else negate a


{-| Calculate the logarithm of a number with a given base.

    logBase 10 100 == 2
    logBase 2 256 == 8
-}
logBase :: Float -> Float -> Float
logBase base n =
    log n / log base


{-| Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
-}
type Order = Ordering


{-| The exclusive-or operator. `True` if exactly one input is `True`. -}
xor :: forall a. (Prelude.BooleanAlgebra a) => a -> a -> a
xor a b =
    (a && not b) || (not a && b)


{-| Truncate a number, rounding towards zero. -}
foreign import truncate :: Float -> Int


{-| Ceiling function, rounding up. -}
ceiling :: Float -> Int
ceiling = Data.Int.ceil


{-| Convert an integer into a float. -}
toFloat :: Int -> Float
toFloat = Data.Int.toNumber


{-| Determine whether a float is positive or negative infinity.

    isInfinite (0/0)     == False
    isInfinite (sqrt -1) == False
    isInfinite (1/0)     == True
    isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.

Note that this is not equivalent to the negation of Javascript's `isFinite()`.
-}
isInfinite :: Float -> Boolean
isInfinite n = 
    n == Global.infinity || n == (-Global.infinity)


{-| Turn any kind of value into a string. When you view the resulting string
with `Text.fromString` it should look just like the value it came from.

    toString 42 == "42"
    toString [1,2] == "[1,2]"
    toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""
-}
toString :: forall a. (Show a) => a -> String
toString = show


-- Function Helpers


infixr 9 <<

{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    not << isEven << sqrt

You can think of this operator as equivalent to the following:

    (g << f)  ==  (\x -> g (f x))

So our example expands out to something like this:

    \n -> not (isEven (sqrt n))
-}
(<<) :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
(<<) = (<<<)


infixl 9 >>

{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    sqrt >> isEven >> not

This direction of function composition seems less pleasant than `(<<)` which
reads nicely in expressions like: `filter (not << isRegistered) students`
-}
(>>) :: forall a b c. (a -> b) -> (b -> c) -> (a -> c)
(>>) = (>>>)


infixl 0 |>

{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
Consider the following code to create a pentagon:

    scale 2 (move (10,10) (filled blue (ngon 5 30)))

This can also be written as:

    ngon 5 30
      |> filled blue
      |> move (10,10)
      |> scale 2
-}
(|>) :: forall a b. a -> (a -> b) -> b
(|>) = (#)


infixr 0 <|

{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis. Consider the following code to create a text element:

    leftAligned (monospace (fromString "code"))

This can also be written as:

    leftAligned << monospace <| fromString "code"
-}
(<|) :: forall a b. (a -> b) -> a -> b
(<|) = ($)


{-| Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).
-}
identity :: forall a. a -> a
identity = id 


{-| Create a [constant function](http://en.wikipedia.org/wiki/Constant_function),
a function that *always* returns the same value regardless of what input you give.
It is defined as:

    always a b = a

It totally ignores the second argument, so `always 42` is a function that always
returns 42. When you are dealing with higher-order functions, this comes in
handy more often than you might expect. For example, creating a zeroed out list
of length ten would be:

    map (always 0) [0..9]
-}
always :: forall a b. a -> b -> a
always = const
