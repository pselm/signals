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

#### `map2`

``` purescript
map2 :: forall a b x value. (a -> b -> value) -> Result x a -> Result x b -> Result x value
```

#### `map3`

``` purescript
map3 :: forall a b c x value. (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
```

#### `map4`

``` purescript
map4 :: forall a b c d x value. (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
```

#### `map5`

``` purescript
map5 :: forall a b c d e x value. (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
```

#### `andThen`

``` purescript
andThen :: forall x a b. Result x a -> (a -> Result x b) -> Result x b
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


