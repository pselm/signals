## Module Elm.Set

A set of unique values. The values can be any type with an
`Ord` instance.

This is implemented in terms of Purescript's `Data.Set`, so
you can also use functions from that module on a `Set`.

#### `remove`

``` purescript
remove :: forall a. (Ord a) => a -> Set a -> Set a
```

Remove a value from a set. If the value is not found, no changes are made.

Equivalent to Purescript's `delete`.

#### `intersect`

``` purescript
intersect :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Get the intersection of two sets. Keeps values that appear in both sets.

Equivalent to Purescript's `intersection`.

#### `diff`

``` purescript
diff :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Get the difference between the first set and the second. Keeps values
that do not appear in the second set.

Equivalent to Purescript's `difference`.

#### `map`

``` purescript
map :: forall a b. (Ord a, Ord b) => (a -> b) -> Set a -> Set b
```

Map a function onto a set, creating a new set with no duplicates.

#### `filter`

``` purescript
filter :: forall a. (Ord a) => (a -> Bool) -> Set a -> Set a
```

Create a new set consisting only of elements which satisfy a predicate.

#### `partition`

``` purescript
partition :: forall a. (Ord a) => (a -> Bool) -> Set a -> { trues :: Set a, falses :: Set a }
```

Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.

Note that the result is a record of `{trues, falses}`, rather than a `Tuple`.


### Re-exported from Data.Foldable:

#### `foldr`

``` purescript
foldr :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

### Re-exported from Data.Set:

#### `Set`

``` purescript
data Set a
```

`Set a` represents a set of values of type `a`

##### Instances
``` purescript
(Eq a) => Eq (Set a)
(Show a) => Show (Set a)
(Ord a) => Ord (Set a)
(Ord a) => Monoid (Set a)
(Ord a) => Semigroup (Set a)
Foldable Set
```

#### `empty`

``` purescript
empty :: forall a. Set a
```

An empty set

#### `fromList`

``` purescript
fromList :: forall a. (Ord a) => List a -> Set a
```

Create a set from a list of elements

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> Set a -> Set a
```

Insert a value into a set

#### `isEmpty`

``` purescript
isEmpty :: forall a. Set a -> Boolean
```

Test if a set is empty

#### `member`

``` purescript
member :: forall a. (Ord a) => a -> Set a -> Boolean
```

Test if a value is a member of a set

#### `singleton`

``` purescript
singleton :: forall a. a -> Set a
```

Create a set with one element

#### `size`

``` purescript
size :: forall a. Set a -> Int
```

Find the size of a set

#### `toList`

``` purescript
toList :: forall a. Set a -> List a
```

Convert a set to a list

#### `union`

``` purescript
union :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Form the union of two sets

Running time: `O(n * log(m))`

### Re-exported from Elm.Foldable:

#### `foldl`

``` purescript
foldl :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

Reduce a container from the left.

   foldl (:) [] [1,2,3] == [3,2,1]

