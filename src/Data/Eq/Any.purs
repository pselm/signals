
module Data.Eq.Any
    ( AnyEq, anyEq
    ) where

import Unsafe.Coerce (unsafeCoerce)
import Data.Eq (class Eq, eq)


-- | A type for values that have forgotten everything but how to determine whether
-- | they are equal to another valu.
newtype AnyEq = AnyEq (∀ b. (∀ a. (Eq a) => a -> b) -> b)


-- | Given a value with an `Eq` instance, forget everything except how to determine
-- | whether you are equal to something else.
anyEq :: ∀ a. (Eq a) => a -> AnyEq
anyEq a = AnyEq \c -> c a


instance eqAnyEq :: Eq AnyEq where
    eq (AnyEq func1) (AnyEq func2) =
        func1 (func2 eqAny)


-- | Check two values for equality, even if all you know is that they both have an
-- | `Eq` instance. Of course, normally you'll know that the values have the same
-- | type, so you'd prefer to just use `(==)` directly.
eqAny :: ∀ a b. (Eq a, Eq b) => a -> b -> Boolean
eqAny a b =
    let
        -- We capture the functions which implement the `Eq` instances.
        eqA = eq :: a -> a -> Boolean
        eqB = eq :: b -> b -> Boolean

    in
        if refEq eqA eqB
            -- If the two `eq` functions are the same, then clearly the function
            -- can accept both a and b. So, we can safely use unsafeCoerce.
            -- Note that we don't necessarily care whether a and b are literally
            -- the same type, so long as they have identical `eq` functions.
            then eqA a (unsafeCoerce b)

            -- If the two `eq` functions are not the same, then clearly the
            -- two values are not equal.
            else false


-- Reference equality, i.e. `a === b` in Javascript.
foreign import refEq :: ∀ a b. a -> b -> Boolean
