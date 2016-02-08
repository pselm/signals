## Module Elm.List

A library for manipulating lists of values. Every value in a
list must have the same type.

Implemented in terms of Purescript's `Data.List`, so you can also
use functions from `Data.List` on a `List`.

#### `(:)`

``` purescript
infixr 5 cons as :
```

_right-associative / precedence 5_

#### `cons`

``` purescript
cons :: forall a. a -> List a -> List a
```

Add an element to the front of a list.

    1 :: [2,3] == [1,2,3]
    1 :: [] == [1]

#### `isEmpty`

``` purescript
isEmpty :: forall a. List a -> Boolean
```

Determine if a list is empty.

   isEmpty [] == True

Equivalent to Purescript's `null`.

#### `member`

``` purescript
member :: forall a. (Eq a) => a -> List a -> Boolean
```

Figure out whether a list contains a value.

    member 9 [1,2,3,4] == False
    member 4 [1,2,3,4] == True

#### `indexedMap`

``` purescript
indexedMap :: forall a b. (Int -> a -> b) -> List a -> List b
```

Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap (,) ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]

#### `scanl`

``` purescript
scanl :: forall a b. (a -> b -> b) -> b -> List a -> List b
```

Reduce a list from the left, building up all of the intermediate results into a list.

    scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]
    
This is like Purescript's `scanl`, except that the function you provide in the first
parameter is flipped, and the second parameter is included in the resulting list.

#### `filterMap`

``` purescript
filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
```

Apply a function that may succeed to all values in the list, but only keep
the successes.

    String.toInt : String -> Maybe Int

    filterMap String.toInt ["3", "4.0", "5", "hats"] == [3,5]

Equivalent to Purescript's `mapMaybe`.

#### `partition`

``` purescript
partition :: forall a. (a -> Boolean) -> List a -> { trues :: List a, falses :: List a }
```

Partition a list based on a predicate. The first list contains all values
that satisfy the predicate, and the second list contains all the value that do
not.

    partition (\x -> x < 3) [0..5] == ([0,1,2], [3,4,5])
    partition isEven        [0..5] == ([0,2,4], [1,3,5])

Note that the result is a record of `{trues, falses}`, whereas in Elm the result
was a `Tuple`.

#### `map2`

``` purescript
map2 :: forall a b result. (a -> b -> result) -> List a -> List b -> List result
```

Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.

    map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]

    map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]

    pairs : List a -> List b -> List (a,b)
    pairs lefts rights =
        map2 (,) lefts rights

Equivalent to Purescript's `zipWith`.

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

Decompose a list of tuples into a tuple of lists.

    unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])

#### `intersperse`

``` purescript
intersperse :: forall a. a -> List a -> List a
```

Places the given value between all members of the given list.

    intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]

#### `repeat`

``` purescript
repeat :: forall a. Int -> a -> List a
```

Create a list with *n* copies of a value:

    repeat 3 (0,0) == [(0,0),(0,0),(0,0)]

Equivalent to Purescript's `replicate`.

#### `sortBy`

``` purescript
sortBy :: forall a comparable. (Ord comparable) => (a -> comparable) -> List a -> List a
```

Sort values by a derived property.

    alice = { name="Alice", height=1.62 }
    bob   = { name="Bob"  , height=1.85 }
    chuck = { name="Chuck", height=1.76 }

    sortBy _.name   [chuck,alice,bob] == [alice,bob,chuck]
    sortBy _.height [chuck,alice,bob] == [alice,chuck,bob]

    sortBy String.length ["mouse","cat"] == ["cat","mouse"]

Note that this is not the same as Purescript's `sortBy`, which is
like Elm's `sortWith`.

#### `sortWith`

``` purescript
sortWith :: forall a. (a -> a -> Ordering) -> List a -> List a
```

Sort values with a custom comparison function.

    sortWith flippedComparison [1..5] == [5,4,3,2,1]

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`

Equivalent to Purescript's `sortBy`.

#### `(..)`

``` purescript
infixl 4 range as ..
```

_left-associative / precedence 4_

#### `range`

``` purescript
range :: Int -> Int -> List Int
```

The Elm built-in range operator `(..)`.

Like Purescript's `range`, except that the Elm version only produces ascending lists.


### Re-exported from Data.Foldable:

#### `foldr`

``` purescript
foldr :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

#### `all`

``` purescript
all :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

Test whether a predicate holds for all elements in a data structure.

#### `any`

``` purescript
any :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

Test whether a predicate holds for any element in a data structure.

#### `maximum`

``` purescript
maximum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the largest element of a structure, according to its `Ord` instance.

#### `minimum`

``` purescript
minimum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the smallest element of a structure, according to its `Ord` instance.

#### `product`

``` purescript
product :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the product of the numeric values in a data structure.

#### `sum`

``` purescript
sum :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the sum of the numeric values in a data structure.

### Re-exported from Data.List:

#### `List`

``` purescript
data List a
  = Nil
  | Cons a (List a)
```

A strict linked list.

A list is either empty (represented by the `Nil` constructor) or non-empty, in
which case it consists of a head element, and another list (represented by the
`Cons` constructor).

##### Instances
``` purescript
(Show a) => Show (List a)
(Eq a) => Eq (List a)
(Ord a) => Ord (List a)
Semigroup (List a)
Monoid (List a)
Functor List
Foldable List
Unfoldable List
Traversable List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadPlus List
```

#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```

Flatten a list of lists.

Running time: `O(n)`, where `n` is the total number of elements.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```

Apply a function to each element in a list, and flatten the results
into a single, new list.

Running time: `O(n)`, where `n` is the total number of elements.

#### `drop`

``` purescript
drop :: forall a. Int -> List a -> List a
```

Drop the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to drop.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```

Filter a list, keeping the elements which satisfy a predicate function.

Running time: `O(n)`

#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```

Get the first element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `length`

``` purescript
length :: forall a. List a -> Int
```

Get the length of a list

Running time: `O(n)`

#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```

Reverse a list.

Running time: `O(n)`

#### `sort`

``` purescript
sort :: forall a. (Ord a) => List a -> List a
```

Sort the elements of an list in increasing order.

#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```

Get all but the first element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `take`

``` purescript
take :: forall a. Int -> List a -> List a
```

Take the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to take.

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

