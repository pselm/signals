## Module Elm.Result

#### `Result`

``` purescript
data Result error value
  = Ok value
  | Err error
```

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

#### `formatError`

``` purescript
formatError :: forall error error' a. (error -> error') -> Result error a -> Result error' a
```

#### `toMaybe`

``` purescript
toMaybe :: forall x a. Result x a -> Maybe a
```

#### `fromMaybe`

``` purescript
fromMaybe :: forall x a. x -> Maybe a -> Result x a
```


