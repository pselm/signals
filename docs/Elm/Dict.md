## Module Elm.Dict

A dictionary mapping unique keys to values. The keys can be any type which
has an instance of the `Ord` class.

This is implemented in terms of Purescript's `Data.Map`.

#### `Dict`

``` purescript
type Dict = Map
```

Elm's `Dict` type is a synonym for Purescript's `Data.Map`.

#### `get`

``` purescript
get :: forall k v. (Ord k) => k -> Dict k v -> Maybe v
```

Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

Equivalent to Purescript's `lookup`.

#### `remove`

``` purescript
remove :: forall k v. (Ord k) => k -> Dict k v -> Dict k v
```

Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

Equivalent to Purescript's `delete`.

#### `update`

``` purescript
update :: forall k v. (Ord k) => k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
```

Update the value of a dictionary for a specific key with a given function.

Like Purescript's `alter`, but with flipped arguments.

#### `intersect`

``` purescript
intersect :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
```

Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.

#### `diff`

``` purescript
diff :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
```

Keep a key-value pair when its key does not appear in the second dictionary.

#### `filter`

``` purescript
filter :: forall k v. (Ord k) => (k -> v -> Bool) -> Dict k v -> Dict k v
```

Keep a key-value pair when it satisfies a predicate.

#### `partition`

``` purescript
partition :: forall k v. (Ord k) => (k -> v -> Bool) -> Dict k v -> { trues :: Dict k v, falses :: Dict k v }
```

Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.

The result is a record of `{trues, falses}`, which is different from the Elm
version, which returns a `Tuple`.

#### `map`

``` purescript
map :: forall k a b. (Ord k) => (k -> a -> b) -> Dict k a -> Dict k b
```

Apply a function to all values in a dictionary.

#### `foldl`

``` purescript
foldl :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
```

Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.

#### `foldr`

``` purescript
foldr :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
```

Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.

#### `toUnfoldable`

``` purescript
toUnfoldable :: forall f k v. (Ord k, Unfoldable f) => Dict k v -> f (Tuple k v)
```

Produce a `Dict` from any `Unfoldable` container of tuples of keys
and values. Defined polymorphically to accommodate Purescript `Array`,
among others.

Note that this is not in the Elm API.


### Re-exported from Data.Map:

#### `empty`

``` purescript
empty :: forall k v. Map k v
```

An empty map

#### `fromFoldable`

``` purescript
fromFoldable :: forall f k v. (Ord k, Foldable f) => f (Tuple k v) -> Map k v
```

Convert any foldable collection of key/value pairs to a map.
On key collision, later values take precedence over earlier ones.

#### `fromList`

``` purescript
fromList :: forall k v. (Ord k) => List (Tuple k v) -> Map k v
```

Create a map from a list of key/value pairs

#### `insert`

``` purescript
insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
```

Insert a key/value pair into a map

#### `isEmpty`

``` purescript
isEmpty :: forall k v. Map k v -> Boolean
```

Test if a map is empty

#### `keys`

``` purescript
keys :: forall k v. Map k v -> List k
```

Get a list of the keys contained in a map

#### `member`

``` purescript
member :: forall k v. (Ord k) => k -> Map k v -> Boolean
```

Test if a key is a member of a map

#### `singleton`

``` purescript
singleton :: forall k v. k -> v -> Map k v
```

Create a map with one key/value pair

#### `size`

``` purescript
size :: forall k v. Map k v -> Int
```

Calculate the number of key/value pairs in a map

#### `toList`

``` purescript
toList :: forall k v. Map k v -> List (Tuple k v)
```

Convert a map to a list of key/value pairs

#### `union`

``` purescript
union :: forall k v. (Ord k) => Map k v -> Map k v -> Map k v
```

Compute the union of two maps, preferring values from the first map in the case
of duplicate keys

#### `values`

``` purescript
values :: forall k v. Map k v -> List v
```

Get a list of the values contained in a map

