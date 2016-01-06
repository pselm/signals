## Module Elm.Array

#### `Array`

``` purescript
type Array = Seq
```

#### `initialize`

``` purescript
initialize :: forall a. Int -> (Int -> a) -> Array a
```

#### `repeat`

``` purescript
repeat :: forall a. Int -> a -> Array a
```

#### `fromList`

``` purescript
fromList :: forall f a. (Foldable f) => f a -> Array a
```

Create an array from a list. 

#### `toList`

``` purescript
toList :: forall f a. (Functor f, Unfoldable f) => Array a -> f a
```

#### `toIndexedList`

``` purescript
toIndexedList :: forall a. Array a -> List (Tuple Int a)
```

#### `indexedMap`

``` purescript
indexedMap :: forall a b. (Int -> a -> b) -> Array a -> Array b
```

#### `foldl`

``` purescript
foldl :: forall a b. (a -> b -> b) -> b -> Array a -> b
```

#### `push`

``` purescript
push :: forall a. a -> Array a -> Array a
```

#### `get`

``` purescript
get :: forall a. Int -> Array a -> Maybe a
```

#### `set`

``` purescript
set :: forall a. Int -> a -> Array a -> Array a
```

#### `slice`

``` purescript
slice :: forall a. Int -> Int -> Array a -> Array a
```


