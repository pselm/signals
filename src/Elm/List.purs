
-- | A library for manipulating lists of values. Every value in a
-- | list must have the same type.
-- |
-- | Implemented in terms of Purescript's `Data.List`, so you can also
-- | use functions from `Data.List` on a `List`.

module Elm.List 
    ( module Virtual
    , cons, (:), isEmpty, member
    , map2, map3, map4, map5
    , intersperse, scanl
    , indexedMap, filterMap, partition, unzip
    , repeat, sortBy, sortWith
    , range, (..)
    ) where


-- For re-export

import Data.List
    ( List(..), head, tail, filter, length, reverse
    , concat, concatMap, take, drop, sort
    ) as Virtual

import Data.Foldable
    ( foldr, all, any, sum, product, maximum, minimum
    ) as Virtual

import Elm.Foldable (foldl) as Virtual

import Prelude (map, append) as Virtual


-- Internal

import Data.List
    ( List(..), elemIndex, length
    , toList, fromList, zipWith, mapMaybe, replicate
    )

import Data.List.ZipList (ZipList(..), runZipList)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Function (on)
import Control.Apply (lift3, lift4, lift5)
import Prelude (class Eq, class Semiring, class Ord, (-), ($), compare, Ordering, flip, (>))


infixr 5 cons as :

-- | Add an element to the front of a list.
-- | 
-- |     1 :: [2,3] == [1,2,3]
-- |     1 :: [] == [1]
cons :: forall a. a -> List a -> List a
cons = Data.List.Cons


-- | Determine if a list is empty.
-- |
-- |    isEmpty [] == True
-- |
-- | Equivalent to Purescript's `null`.
isEmpty :: forall a. List a -> Boolean
isEmpty = Data.List.null


-- | Figure out whether a list contains a value.
-- | 
-- |     member 9 [1,2,3,4] == False
-- |     member 4 [1,2,3,4] == True
member :: forall a. (Eq a) => a -> List a -> Boolean
member x xs =
    case elemIndex x xs of
         Just _ -> true
         Nothing -> false


-- | Same as `map` but the function is also applied to the index of each
-- | element (starting at zero).
-- | 
-- |     indexedMap (,) ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
indexedMap :: forall a b. (Int -> a -> b) -> List a -> List b
indexedMap func list =
    zipWith func r list
        where
            r =
                range 0 (length list - 1)


-- | Reduce a list from the left, building up all of the intermediate results into a list.
-- | 
-- |     scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]
-- |     
-- | This is like Purescript's `scanl`, except that the function you provide in the first
-- | parameter is flipped, and the second parameter is included in the resulting list.
scanl :: forall a b. (a -> b -> b) -> b -> List a -> List b
scanl func memo list =
    memo : Data.Traversable.scanl (flip func) memo list


-- | Apply a function that may succeed to all values in the list, but only keep
-- | the successes.
-- | 
-- |     String.toInt : String -> Maybe Int
-- | 
-- |     filterMap String.toInt ["3", "4.0", "5", "hats"] == [3,5]
-- |
-- | Equivalent to Purescript's `mapMaybe`.
filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
filterMap = mapMaybe


-- | Partition a list based on a predicate. The first list contains all values
-- | that satisfy the predicate, and the second list contains all the value that do
-- | not.
-- | 
-- |     partition (\x -> x < 3) [0..5] == ([0,1,2], [3,4,5])
-- |     partition isEven        [0..5] == ([0,2,4], [1,3,5])
-- |
-- | Note that the result is a record of `{trues, falses}`, whereas in Elm the result
-- | was a `Tuple`.
partition :: forall a. (a -> Boolean) -> List a -> { trues :: List a, falses :: List a }
partition pred list =
    foldr step { trues: Nil, falses: Nil } list 
        where
            step x memo =
                if pred x
                    then memo { trues = x : memo.trues }
                    else memo { falses = x : memo.falses }


-- | Combine two lists, combining them with the given function.
-- | If one list is longer, the extra elements are dropped.
-- | 
-- |     map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]
-- | 
-- |     map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
-- | 
-- |     pairs : List a -> List b -> List (a,b)
-- |     pairs lefts rights =
-- |         map2 (,) lefts rights
-- |
-- | Equivalent to Purescript's `zipWith`.
map2 :: forall a b result. (a -> b -> result) -> List a -> List b -> List result
map2 = zipWith


map3 :: forall a b c result. (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 func list1 list2 list3 =
    toList $ runZipList $ lift3 func (ZipList $ fromList list1) (ZipList $ fromList list2) (ZipList $ fromList list3)


map4 :: forall a b c d result. (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 func list1 list2 list3 list4 =
    toList $ runZipList $ lift4 func (ZipList $ fromList list1) (ZipList $ fromList list2) (ZipList $ fromList list3) (ZipList $ fromList list4)


map5 :: forall a b c d e result. (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 func list1 list2 list3 list4 list5 =
    toList $ runZipList $ lift5 func (ZipList $ fromList list1) (ZipList $ fromList list2) (ZipList $ fromList list3) (ZipList $ fromList list4) (ZipList $ fromList list5)


-- | Decompose a list of tuples into a tuple of lists.
-- | 
-- |     unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip pairs =
    let
        step (Tuple x y) (Tuple xs ys) =
            Tuple (x : xs) (y : ys)

    in
        foldr step (Tuple Nil Nil) pairs


-- | Places the given value between all members of the given list.
-- |
-- |     intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
intersperse :: forall a. a -> List a -> List a
intersperse sep xs =
    case xs of
        Nil -> Nil

        Cons hd tl ->
            let
                step x rest =
                    sep : x : rest

                spersed =
                    foldr step Nil tl

            in
                hd : spersed


-- | Create a list with *n* copies of a value:
-- | 
-- |     repeat 3 (0,0) == [(0,0),(0,0),(0,0)]
-- |
-- | Equivalent to Purescript's `replicate`.
repeat :: forall a. Int -> a -> List a
repeat = replicate


-- | Sort values by a derived property.
-- | 
-- |     alice = { name="Alice", height=1.62 }
-- |     bob   = { name="Bob"  , height=1.85 }
-- |     chuck = { name="Chuck", height=1.76 }
-- | 
-- |     sortBy _.name   [chuck,alice,bob] == [alice,bob,chuck]
-- |     sortBy _.height [chuck,alice,bob] == [alice,chuck,bob]
-- | 
-- |     sortBy String.length ["mouse","cat"] == ["cat","mouse"]
-- |
-- | Note that this is not the same as Purescript's `sortBy`, which is
-- | like Elm's `sortWith`.
sortBy :: forall a comparable. (Ord comparable) => (a -> comparable) -> List a -> List a
sortBy func =
    Data.List.sortBy (compare `on` func)


-- | Sort values with a custom comparison function.
-- | 
-- |     sortWith flippedComparison [1..5] == [5,4,3,2,1]
-- | 
-- |     flippedComparison a b =
-- |         case compare a b of
-- |           LT -> GT
-- |           EQ -> EQ
-- |           GT -> LT
-- | 
-- | This is also the most general sort function, allowing you
-- | to define any other: `sort == sortWith compare`
-- |
-- | Equivalent to Purescript's `sortBy`.
sortWith :: forall a. (a -> a -> Ordering) -> List a -> List a
sortWith = Data.List.sortBy


infixl 4 range as ..

-- | The Elm built-in range operator `(..)`.
-- |
-- | Like Purescript's `range`, except that the Elm version only produces ascending lists.
range :: Int -> Int -> List Int
range low high =
    if low > high
        then Nil
        else Data.List.range low high
