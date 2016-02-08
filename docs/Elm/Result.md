## Module Elm.Result

A `Result` is the result of a computation that may fail.

Normally, I would have wanted to implement this in terms of Purescript's
`Either` module, since it is essentially equivalent.

However, the difficulty is that there is no way to alias the data constructors
`Left` and `Right`, so that you could use Elm's `Ok` and `Err` instead.
So, in order to require fewer changes to code coming from Elm, I've
implemented a separate `Result` type here.

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
```

#### `withDefault`

``` purescript
withDefault :: forall x a. a -> Result x a -> a
```

If the result is `Ok` return the value, but if the result is an `Err` then
return a given default value. The following examples try to parse integers.

    Result.withDefault 0 (String.toInt "123") == 123
    Result.withDefault 0 (String.toInt "abc") == 0

#### `formatError`

``` purescript
formatError :: forall error error' a. (error -> error') -> Result error a -> Result error' a
```

Format the error value of a result. If the result is `Ok`, it stays exactly
the same, but if the result is an `Err` we will format the error. For example,
say the errors we get have too much information:

    parseInt : String -> Result ParseError Int

    type ParseError =
        { message : String
        , code : Int
        , position : (Int,Int)
        }

    formatError .message (parseInt "123") == Ok 123
    formatError .message (parseInt "abc") == Err "char 'a' is not a number"

Equivalent to Purescript's `lmap`.

#### `toMaybe`

``` purescript
toMaybe :: forall x a. Result x a -> Maybe a
```

Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

    parseInt : String -> Result ParseError Int

    maybeParseInt : String -> Maybe Int
    maybeParseInt string =
        toMaybe (parseInt string)

#### `fromMaybe`

``` purescript
fromMaybe :: forall x a. x -> Maybe a -> Result x a
```

Convert from a simple `Maybe` to interact with some code that primarily
uses `Results`.

    parseInt : String -> Maybe Int

    resultParseInt : String -> Result String Int
    resultParseInt string =
        fromMaybe ("error parsing string: " ++ toString string) (parseInt string)


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

