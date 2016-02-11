
-- | A `Result` is the result of a computation that may fail.
-- |
-- | Normally, I would have wanted to implement this in terms of Purescript's
-- | `Either` module, since it is essentially equivalent.
-- | 
-- | However, the difficulty is that there is no way to alias the data constructors
-- | `Left` and `Right`, so that you could use Elm's `Ok` and `Err` instead.
-- | So, in order to require fewer changes to code coming from Elm, I've
-- | implemented a separate `Result` type here.

module Elm.Result
    ( module Virtual
    , Result(Ok, Err)
    , withDefault
    , toMaybe, fromMaybe, formatError
    ) where


-- This is mostly adapted from https://github.com/purescript/purescript-either/blob/master/src/Data/Either.purs


-- For re-export

import Prelude (map) as Virtual
import Elm.Apply (map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual


-- Internal

import Prelude
    ( class Functor, (<$>)
    , class Apply, pure
    , class Applicative, (<*>)
    , class Bind, class Monad, bind
    , class Semiring, (++), one, zero, add
    , class Semigroup, append, mul
    , class Bounded, top, bottom
    , class Show, show
    , class Eq, (==)
    , class Ord, Ordering(..), compare
    )

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Bitraversable (class Bitraversable)
import Data.Foldable (class Foldable)
import Data.Monoid (mempty)
import Data.Traversable (class Traversable)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Elm.Maybe (Maybe (Just, Nothing))


-- | A `Result` is either `Ok` meaning the computation succeeded, or it is an
-- | `Err` meaning that there was some failure.
data Result error value
    = Ok value
    | Err error


-- | If the result is `Ok` return the value, but if the result is an `Err` then
-- | return a given default value. The following examples try to parse integers.
-- | 
-- |     Result.withDefault 0 (String.toInt "123") == 123
-- |     Result.withDefault 0 (String.toInt "abc") == 0
withDefault :: forall x a. a -> Result x a -> a
withDefault _ (Ok a)  = a
withDefault d (Err _) = d


-- | Format the error value of a result. If the result is `Ok`, it stays exactly
-- | the same, but if the result is an `Err` we will format the error. For example,
-- | say the errors we get have too much information:
-- | 
-- |     parseInt : String -> Result ParseError Int
-- | 
-- |     type ParseError =
-- |         { message : String
-- |         , code : Int
-- |         , position : (Int,Int)
-- |         }
-- | 
-- |     formatError .message (parseInt "123") == Ok 123
-- |     formatError .message (parseInt "abc") == Err "char 'a' is not a number"
-- |
-- | Equivalent to Purescript's `lmap`.
formatError :: forall error error' a. (error -> error') -> Result error a -> Result error' a
formatError = lmap


-- | Convert to a simpler `Maybe` if the actual error message is not needed or
-- | you need to interact with some code that primarily uses maybes.
-- | 
-- |     parseInt : String -> Result ParseError Int
-- | 
-- |     maybeParseInt : String -> Maybe Int
-- |     maybeParseInt string =
-- |         toMaybe (parseInt string)
toMaybe :: forall x a. Result x a -> Maybe a
toMaybe (Ok v)  = Just v
toMaybe (Err _) = Nothing


-- | Convert from a simple `Maybe` to interact with some code that primarily
-- | uses `Results`.
-- | 
-- |     parseInt : String -> Maybe Int
-- | 
-- |     resultParseInt : String -> Result String Int
-- |     resultParseInt string =
-- |         fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
fromMaybe :: forall x a. x -> Maybe a -> Result x a
fromMaybe _ (Just v) = Ok v
fromMaybe err Nothing = Err err


-- Like Purescript's `either`.
result :: forall a b c. (a -> c) -> (b -> c) -> Result a b -> c
result f _ (Err a) = f a
result _ g (Ok b)  = g b


instance functorResult :: Functor (Result a) where
    map _ (Err x) = Err x
    map f (Ok y)  = Ok (f y)

instance bifunctorResult :: Bifunctor Result where
    bimap f _ (Err l) = Err (f l)
    bimap _ g (Ok r)  = Ok (g r)

instance applyResult :: Apply (Result e) where
    apply (Err e) _ = Err e
    apply (Ok f) r  = f <$> r

instance applicativeResult :: Applicative (Result e) where
    pure = Ok

instance altResult :: Alt (Result e) where
    alt (Err _) r = r
    alt l       _ = l

instance bindResult :: Bind (Result e) where
    bind = result (\e _ -> Err e) (\a f -> f a)

instance monadResult :: Monad (Result e)

instance extendResult :: Extend (Result e) where
    extend _ (Err y)  = Err y
    extend f x        = Ok (f x)

instance showResult :: (Show a, Show b) => Show (Result a b) where
    show (Err x) = "Err (" ++ show x ++ ")"
    show (Ok y)  = "Ok (" ++ show y ++ ")"

instance eqResult :: (Eq a, Eq b) => Eq (Result a b) where
    eq (Err a1) (Err a2)  = a1 == a2
    eq (Ok b1)  (Ok b2)   = b1 == b2
    eq _          _       = false

instance ordResult :: (Ord a, Ord b) => Ord (Result a b) where
    compare (Err a1) (Err a2) = compare a1 a2
    compare (Ok b1)  (Ok b2)  = compare b1 b2
    compare (Err a)  _        = LT
    compare _        (Err b)  = GT

instance boundedResult :: (Bounded a, Bounded b) => Bounded (Result a b) where
    top = Ok top
    bottom = Err bottom

instance foldableResult :: Foldable (Result a) where
    foldr _ z (Err _) = z
    foldr f z (Ok x)  = f x z
    foldl _ z (Err _) = z
    foldl f z (Ok x)  = f z x
    foldMap f (Err _) = mempty
    foldMap f (Ok x)  = f x

instance bifoldableResult :: Bifoldable Result where
    bifoldr f _ z (Err a) = f a z
    bifoldr _ g z (Ok b)  = g b z
    bifoldl f _ z (Err a) = f z a
    bifoldl _ g z (Ok b)  = g z b
    bifoldMap f _ (Err a) = f a
    bifoldMap _ g (Ok b)  = g b

instance traversableResult :: Traversable (Result a) where
    traverse _ (Err x) = pure (Err x)
    traverse f (Ok x)  = Ok <$> f x
    sequence (Err x)   = pure (Err x)
    sequence (Ok x)    = Ok <$> x

instance bitraversableResult :: Bitraversable Result where
    bitraverse f _ (Err a) = Err <$> f a
    bitraverse _ g (Ok b)  = Ok <$> g b
    bisequence (Err a) = Err <$> a
    bisequence (Ok b)  = Ok <$> b

instance semiringResult :: (Semiring b) => Semiring (Result a b) where
    one = Ok one
    mul x y = mul <$> x <*> y
    zero = Ok zero
    add x y = add <$> x <*> y

instance semigroupResult :: (Semigroup b) => Semigroup (Result a b) where
    append x y = append <$> x <*> y

instance arbitraryResult :: (Arbitrary a, Arbitrary b) => Arbitrary (Result a b) where
    arbitrary = do
        b <- arbitrary
        if b
            then Err <$> arbitrary
            else Ok <$> arbitrary

instance coarbitraryResult :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Result a b) where
    coarbitrary (Err a) = coarbitrary a
    coarbitrary (Ok b) = coarbitrary b
