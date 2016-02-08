## Module Elm.Maybe

This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

This is implemented in terms of Purescript's `Data.Maybe`, so you can use functions
from there on `Maybe` values as well.

One of the neat things about Purescript's `Data.Maybe` is that it has an
instance for various type-classes if the underlying type has an instance.
So, suppose you have two `Maybe Int` values. You can actually just add them,
and you'll get the expected result. (That is, a `Maybe Int` which is the sum
if both arguments were `Just`, or `Nothing` otherwise). E.g.

    (Just 7) + (Just 3) == Just 10
    (Just 7) + Nothing == Nothing

#### `withDefault`

``` purescript
withDefault :: forall a. a -> Maybe a -> a
```

Provide a default value, turning an optional value into a normal
value.  This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.

    withDefault 100 (Just 42)   -- 42
    withDefault 100 Nothing     -- 100

    withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"

Equivalent to Purescript's 'fromMaybe`.

#### `oneOf`

``` purescript
oneOf :: forall a. List (Maybe a) -> Maybe a
```

Pick the first `Maybe` that actually has a value. Useful when you want to
try a couple different things, but there is no default value.

    oneOf [ Nothing, Just 42, Just 71 ] == Just 42
    oneOf [ Nothing, Nothing, Just 71 ] == Just 71
    oneOf [ Nothing, Nothing, Nothing ] == Nothing


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

### Re-exported from Elm.Apply:

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

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. (Functor f) => (a -> b) -> f a -> f b
```

