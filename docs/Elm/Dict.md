## Module Elm.Dict

#### `Dict`

``` purescript
type Dict = Map
```

#### `get`

``` purescript
get :: forall k v. (Ord k) => k -> Dict k v -> Maybe v
```

#### `remove`

``` purescript
remove :: forall k v. (Ord k) => k -> Dict k v -> Dict k v
```

#### `update`

``` purescript
update :: forall k v. (Ord k) => k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
```

Update the value of a dictionary for a specific key with a given function. 

#### `intersect`

``` purescript
intersect :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
```

#### `diff`

``` purescript
diff :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
```

Keep a key-value pair when its key does not appear in the second dictionary.

#### `filter`

``` purescript
filter :: forall k v. (Ord k) => (k -> v -> Boolean) -> Dict k v -> Dict k v
```

Keep a key-value pair when it satisfies a predicate. 

#### `partition`

``` purescript
partition :: forall k v. (Ord k) => (k -> v -> Boolean) -> Dict k v -> { trues :: Dict k v, falses :: Dict k v }
```

#### `map`

``` purescript
map :: forall k a b. (Ord k) => (k -> a -> b) -> Dict k a -> Dict k b
```

Apply a function to all values in a dictionary. 

#### `foldl`

``` purescript
foldl :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
```

#### `foldr`

``` purescript
foldr :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
```


