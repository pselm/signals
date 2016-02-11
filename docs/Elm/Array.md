## Module Elm.Array

A library for fast immutable arrays. The elements in an array must have the
same type.

This is based on the Purescript `Data.Sequence` package -- an `Array` is a
`Data.Sequence.Seq`, so you can use additional functions from that package
as well.

Note that the Purescript primitive `Array` type is something different --
it is actually a Javascript array.

#### `Array`

``` purescript
type Array = Seq
```

The `Array` type is synonym for `Data.Sequence.Seq`.

#### `initialize`

``` purescript
initialize :: forall a. Int -> (Int -> a) -> Array a
```

Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

    initialize 4 identity    == fromList [0,1,2,3]
    initialize 4 (\n -> n*n) == fromList [0,1,4,9]
    initialize 4 (always 0)  == fromList [0,0,0,0]

#### `repeat`

``` purescript
repeat :: forall a. Int -> a -> Array a
```

Creates an array with a given length, filled with a default element.

    repeat 5 0     == fromList [0,0,0,0,0]
    repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (, (-)always x)`.

#### `fromList`

``` purescript
fromList :: forall f a. (Foldable f) => f a -> Array a
```

Create an array from a list.

Note that this actually works with any `Foldable`.

#### `toList`

``` purescript
toList :: forall f a. (Functor f, Unfoldable f) => Array a -> f a
```

Create a list of elements from an array.

    toList (fromList [3,5,8]) == [3,5,8]

Note that this actually works with any type that is both a
`Functor` and an `Unfoldable`.

#### `toIndexedList`

``` purescript
toIndexedList :: forall f a. (Applicative f, Monoid (f (Tuple Int a))) => Array a -> f (Tuple Int a)
```

Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]

The container in the return type is defined polymorphically to accommodate
`List` and Purescript's `Array`, among others.

#### `indexedMap`

``` purescript
indexedMap :: forall a b. (Int -> a -> b) -> Array a -> Array b
```

Apply a function on every element with its index as first argument.

    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]

#### `push`

``` purescript
push :: forall a. a -> Array a -> Array a
```

Push an element to the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]

#### `get`

``` purescript
get :: forall a. Int -> Array a -> Maybe a
```

Return Just the element at the index or Nothing if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing

#### `set`

``` purescript
set :: forall a. Int -> a -> Array a -> Array a
```

Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]

#### `slice`

``` purescript
slice :: forall a. Int -> Int -> Array a -> Array a
```

Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`.

    slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
    slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

    slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
    slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]

This makes it pretty easy to `pop` the last element off of an array: `slice 0 -1 array`


### Re-exported from Data.Foldable:

#### `foldr`

``` purescript
foldr :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

### Re-exported from Data.Sequence:

#### `empty`

``` purescript
empty :: forall a. Seq a
```

A sequence with no elements.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> Seq a -> Seq a
```

O(n). Create a new Seq which contains only those elements of the input
Seq which satisfy the given predicate.

#### `length`

``` purescript
length :: forall a. Seq a -> Int
```

O(1). The number of elements in the sequence.

### Re-exported from Elm.Foldable:

#### `foldl`

``` purescript
foldl :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

Reduce a container from the left.

   foldl (:) [] [1,2,3] == [3,2,1]

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. (Functor f) => (a -> b) -> f a -> f b
```

#### `append`

``` purescript
append :: forall a. (Semigroup a) => a -> a -> a
```

