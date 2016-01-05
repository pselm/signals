## Module Elm.Set

#### `remove`

``` purescript
remove :: forall a. (Ord a) => a -> Set a -> Set a
```

Remove a value from a set. If the value is not found, no changes are made. 

#### `intersect`

``` purescript
intersect :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Get the intersection of two sets. Keeps values that appear in both sets. 

#### `diff`

``` purescript
diff :: forall a. (Ord a) => Set a -> Set a -> Set a
```

#### `map`

``` purescript
map :: forall a b. (Ord a, Ord b) => (a -> b) -> Set a -> Set b
```

Map a function onto a set, creating a new set with no duplicates. 

#### `foldl`

``` purescript
foldl :: forall a b. (Ord a) => (a -> b -> b) -> b -> Set a -> b
```

Fold over the values in a set, in order from lowest to highest. 

#### `filter`

``` purescript
filter :: forall a. (Ord a) => (a -> Boolean) -> Set a -> Set a
```

Create a new set consisting only of elements which satisfy a predicate. 

#### `partition`

``` purescript
partition :: forall a. (Ord a) => (a -> Boolean) -> Set a -> { trues :: Set a, falses :: Set a }
```


