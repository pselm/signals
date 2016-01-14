## Module Elm.List

#### `(:)`

``` purescript
(:) :: forall a. a -> List a -> List a
```

_right-associative / precedence 5_

#### `isEmpty`

``` purescript
isEmpty :: forall a. List a -> Boolean
```

#### `member`

``` purescript
member :: forall a. (Eq a) => a -> List a -> Boolean
```

#### `indexedMap`

``` purescript
indexedMap :: forall a b. (Int -> a -> b) -> List a -> List b
```

#### `foldl`

``` purescript
foldl :: forall a b. (a -> b -> b) -> b -> List a -> b
```

#### `scanl`

``` purescript
scanl :: forall a b. (a -> b -> b) -> b -> List a -> List b
```

#### `filterMap`

``` purescript
filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
```

#### `partition`

``` purescript
partition :: forall a. (a -> Boolean) -> List a -> { trues :: List a, falses :: List a }
```

#### `map2`

``` purescript
map2 :: forall a b result. (a -> b -> result) -> List a -> List b -> List result
```

#### `map3`

``` purescript
map3 :: forall a b c result. (a -> b -> c -> result) -> List a -> List b -> List c -> List result
```



#### `map4`

``` purescript
map4 :: forall a b c d result. (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
```



#### `map5`

``` purescript
map5 :: forall a b c d e result. (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
```



#### `unzip`

``` purescript
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
```

#### `intersperse`

``` purescript
intersperse :: forall a. a -> List a -> List a
```

#### `repeat`

``` purescript
repeat :: forall a. Int -> a -> List a
```

#### `sortBy`

``` purescript
sortBy :: forall a comparable. (Ord comparable) => (a -> comparable) -> List a -> List a
```

#### `sortWith`

``` purescript
sortWith :: forall a. (a -> a -> Ordering) -> List a -> List a
```

#### `(..)`

``` purescript
(..) :: Int -> Int -> List Int
```

_left-associative / precedence -1_


