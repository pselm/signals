module Elm.Array 
    ( module Virtual
    , Array(), fromList, toList, toIndexedList
    , repeat, foldl, push, get, set
    , slice, indexedMap, initialize
    ) where


-- For re-export

import Data.Sequence (map, filter, empty, length) as Virtual
import Data.Foldable (foldr) as Virtual
import Prelude (append) as Virtual


-- Internal

import Data.Sequence
    ( Seq(), toUnfoldable, fromFoldable, empty, replace
    , snoc, index, null, take, drop, length
    )

import Data.Tuple (Tuple(..))
import Prelude (Functor, (<$>), ($), (-), const, flip, (>=), (<=), (+), (<<<))
import Data.Foldable (Foldable)
import Data.Unfoldable (Unfoldable)
import Data.List (List(..), (:), reverse)
import Data.Ord (clamp)
import Data.Maybe (Maybe())
import Data.Tuple (Tuple(..), snd)
import Prim hiding (Array())


type Array = Seq


{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

    initialize 4 identity    == fromList [0,1,2,3]
    initialize 4 (\n -> n*n) == fromList [0,1,4,9]
    initialize 4 (always 0)  == fromList [0,0,0,0]
-}
initialize :: forall a. Int -> (Int -> a) -> Array a
initialize len func =
    if len <= 0
       then empty
       else
            func <$>
               fromFoldable (Data.Array.range 0 (len - 1))


{-| Creates an array with a given length, filled with a default element.

    repeat 5 0     == fromList [0,0,0,0,0]
    repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (, (-)always x)`.
-}
repeat :: forall a. Int -> a -> Array a
repeat n e =
    initialize n (const e)


{-| Create an array from a list. -}
fromList :: forall f a. (Foldable f) => f a -> Array a
fromList = fromFoldable


{-| Create a list of elements from an array.

    toList (fromList [3,5,8]) == [3,5,8]
-}
toList :: forall f a. (Functor f, Unfoldable f) => Array a -> f a
toList = toUnfoldable


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
-}
toIndexedList :: forall a. Array a -> List (Tuple Int a)
toIndexedList =
    reverse <<< snd <<< Data.Foldable.foldl step (Tuple 0 Nil)
        where
            step (Tuple index list) item =
                Tuple (index + 1) (Tuple index item : list) 


{-| Apply a function on every element with its index as first argument.

    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
-}
indexedMap :: forall a b. (Int -> a -> b) -> Array a -> Array b
indexedMap func =
    snd <<< Data.Foldable.foldl step (Tuple 0 empty)   
        where
            step (Tuple index seq) item =
                Tuple (index + 1) (snoc seq (func index item)) 

    
{-| Reduce an array from the left. Read `foldl` as &ldquo;fold from the left&rdquo;.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl :: forall a b. (a -> b -> b) -> b -> Array a -> b
foldl func =
    Data.Foldable.foldl (flip func)


{-| Push an element to the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]
-}
push :: forall a. a -> Array a -> Array a
push = flip snoc


{-| Return Just the element at the index or Nothing if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing

-}
get :: forall a. Int -> Array a -> Maybe a
get = index


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set :: forall a. Int -> a -> Array a -> Array a
set = flip replace


{-| Get a sub-section of an array: `(slice start end array)`. The `start` is a
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
-}
slice :: forall a. Int -> Int -> Array a -> Array a
slice start end array =
    let
        len =
            length array

        clamper =
            clamp 0 len

        toDrop =
            clamper $
                if start >= 0
                    then start
                    else len + start

        toTake =
            clamper $
                if end >= 0
                    then end - toDrop
                    else len + end - toDrop

     in
        take toTake $ drop toDrop array


{-| Determine if an array is empty.

    isEmpty empty == True
-}
isEmpty :: forall a. Array a -> Boolean
isEmpty = null
