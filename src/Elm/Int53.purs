
-- | In Purescript, the `Int` type is restricted to 32-bit integers. However,
-- | the underlying Javascript runtime is capable of working with 53-bit
-- | integers. So, this module exposes an `Int53` type, for cases where you
-- | want all 53 bits.
-- |
-- | This is needed for some code ported from Elm, since the Elm `Int` type
-- | can handle 53 bits.

module Elm.Int53
    ( Int53
    , class Int53Value, toInt53, fromInt53
    , fromNumber, toNumber
    , fromInt, toInt
    , fromString
    , ceil, floor, round, truncate
    , even, odd
    ) where


import Prelude
    ( class Semiring, class Ring, class ModuloSemiring, class Eq
    , class Bounded, class Ord, class BoundedOrd, class Show
    , (+), (-), (*), (/), (==), ($), (++), (>), compare, show, top, bottom
    , negate, (<), (<<<), (/=), (||), id, bind, pure
    )

import Math as Math
import Elm.Basics (class Pow, pow)
import Global (readFloat, isNaN)
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)


-- | A JavaScript 53-bit signed integer.
newtype Int53 = Int53 Number


instance semiringInt53 :: Semiring Int53 where
    add (Int53 a) (Int53 b) = Int53 (a + b)
    zero = Int53 0.0
    mul (Int53 a) (Int53 b) = Int53 (a * b)
    one = Int53 1.0


instance ringInt53 :: Ring Int53 where
    sub (Int53 a) (Int53 b) = Int53 (a - b)


instance moduloSemiringInt53 :: ModuloSemiring Int53 where
    div (Int53 a) (Int53 b) = truncate $ a / b
    mod (Int53 a) (Int53 b) = Int53 (Math.(%) a b)


instance eqInt53 :: Eq Int53 where
    eq (Int53 a) (Int53 b) = a == b


instance ordInt53 :: Ord Int53 where
    compare (Int53 a) (Int53 b) = compare a b


instance boundedInt53 :: Bounded Int53 where
    top = Int53 topFloat
    bottom = Int53 bottomFloat

topFloat :: Number
topFloat = 9007199254740991.0

bottomFloat :: Number
bottomFloat = -9007199254740991.0


instance boundedOrdInt53 :: BoundedOrd Int53 where


instance showInt53 :: Show Int53 where
    show (Int53 a) = "(truncate " ++ show a ++ ")"


instance powInt53 :: Pow Int53 where
    pow (Int53 a) (Int53 b) = Int53 (pow a b)


instance arbitraryInt53 :: Arbitrary Int53 where
    arbitrary = do
        -- This obviously isn't the full range of Int53.
        -- However, arbitraryInt does the same thing, and
        -- probably for the same reason ... which is to
        -- avoid quickcheck tests that overflow.
        -- TODO: Should adjust those tests, and then make
        -- this range bigger.
        n <- choose 94906265.0 (-94906265.0)
        pure $ truncate n


-- Clamps to the top and bottom. Unsafe because it assumes that
-- something has already been done to remove any fractional part.
unsafeClamp :: Number -> Int53
unsafeClamp a =
    if a > topFloat
        then top
        else
            if a < bottomFloat
                then bottom
                else Int53 a


{- This is the key difference from ordinary integers. For ordinary
integers, Purescript does a Javascript "a | 0", which is what
limits ordinary integers to 32 bits.

The other difference is the Purescript is more aggressive about
actually doing the truncating. We only do it where the underlying
operations could produce a fractional part ... that is, we assume
that the type system is doing its job.
-}

-- | Convert a `Number` to an `Int53`, by rounding towards zero.
-- | Values outside the `Int53` range are clamped.
truncate :: Number -> Int53
truncate a =
    if a > 0.0
        then floor a
        else ceil a


-- | Convert a `Number` to an `Int53`, by taking the closest integer equal to or
-- | less than the argument. Values outside the `Int53` range are clamped.
floor :: Number -> Int53
floor = unsafeClamp <<< Math.floor


-- | Convert a `Number` to an `Int53`, by taking the closest integer equal to or
-- | greater than the argument. Values outside the `Int53` range are clamped.
ceil :: Number -> Int53
ceil = unsafeClamp <<< Math.ceil


-- | Convert a `Number` to an `Int53`, by taking the nearest integer to the
-- | argument. Values outside the `Int53` range are clamped.
round :: Number -> Int53
round = unsafeClamp <<< Math.round


-- | Creates an `Int53` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int53` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Int53
fromNumber a =
    if isNaN a || a > topFloat || a < bottomFloat
        then Nothing
        else
            if Math.floor a == a
                then Just $ Int53 a
                else Nothing


-- | Converts an `Int53` value back into a `Number`. Any `Int53` is a valid `Number`
-- | so there is no loss of precision with this function.
toNumber :: Int53 -> Number
toNumber (Int53 a) = a


-- | Reads an `Int53` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int53` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Int53
fromString = fromNumber <<< readFloat


-- | Converts an `Int` to an `Int53`.
fromInt :: Int -> Int53
fromInt = Int53 <<< Data.Int.toNumber


-- | Converts an `Int53` to an `Int`. Values outside the `Int` range are clamped.
toInt :: Int53 -> Int
toInt (Int53 a) =
    -- Calling `floor` gets us a cheap clamp ... of course, we shouldn't need the floor
    Data.Int.floor a


-- | Returns whether an `Int53` is an even number.
-- |
-- |     even (fromInt 0) == true
-- |     even (fromInt 1) == false
even :: Int53 -> Boolean
even (Int53 a) =
    Math.(%) a 2.0 == 0.0


-- | The negation of `even`.
-- |
-- |     odd (fromInt 0) == false
-- |     odd (fromInt 1) == true
odd :: Int53 -> Boolean
odd (Int53 a) =
    Math.(%) a 2.0 /= 0.0


-- | A class for cases where we'd like to accept eitner `Int` or `Int53`,
-- | work with `Int53` internally, and then return whatever type we were
-- | given. The conversions should do their best ... for instance, they
-- | may clamp if necessary, or truncate etc.
class Int53Value a where
    toInt53 :: a -> Int53
    fromInt53 :: Int53 -> a


instance int53Int53Value :: Int53Value Int53 where
    toInt53 = id
    fromInt53 = id


instance intInt53Value :: Int53Value Int where
    toInt53 = fromInt
    fromInt53 = toInt
