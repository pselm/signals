module Test.UnqualifiedImport where

{- This is a test to see how much of the other modules can be imported
unqualified. Not that this is recommended, but it is nice to be able to
support it as much as is reasonable.
-}

import Elm.Basics

-- `Array` conflicts with `Prim.Array`
import Elm.Array hiding (Array())

-- `xor` inevitably conflicts with `Elm.Basics.xor`
import Elm.Bitwise hiding (xor)

-- `toLower` and `toUpper` conflict with String
import Elm.Char hiding (toLower, toUpper)

import Elm.Date
import Elm.Debug

{- I don't think toList or fromList can be unified with the other toList's and
fromList's, because they have a double type ... e.g. Map k v -> List (Tuple k v)

Same issue with `map`, `foldl`, `foldr`, `filter` and `partition`

TODO: Perhaps `Elm.Array.get` and `Elm.Dict.get` could be unified.

`empty` is an interesting case. I could probably unify `empty` using `Monoid`.
That would allow importing, say, `Elm.Array` and `Elm.Dict` together. However,
the underlying Purescript classes (`Data.Seq` and `Data.Map`) define their own
`empty`, which I wouldn't be able to import. So, it's a matter of picking my
poison.
-}
import Elm.Dict hiding (toList, fromList, empty, get, filter, map, foldl, foldr, partition)

{- `fromString` here is a `Maybe`, whereas in `Elm.Date` it is a `Result`.

`ceil`, `floor` and `round` conflict with `Data.Int`. Now, I do have a
`LikeInt53` class already ... perhaps I should locate these there? I suppose
the problem would be if one then tried to import `Data.Int`. That wouldn't be a
problem for `truncate`. 
-}
import Elm.Int53 hiding (fromString, ceil, floor, round, truncate)

{- There are a number of things that could conceivably be unified between
`Elm.List` and `Elm.Array` ... `length`, `filter`, `foldl`, `indexedMap`,
`repeat`

It should be possible to unify `isEmpty`, `member` as between `Dict` and `List`.
-}
import Elm.List hiding
    ( length, filter, isEmpty, member, foldl, indexedMap, repeat
    -- The mapping functions are not the normal apply in List
    , map2, map3, map4, map5
    )

import Elm.Maybe
import Elm.Random

-- Conflicts with String
import Elm.Regex hiding (contains, split)

{- Possibly unify `withDefault` with `Maybe` -}
import Elm.Result hiding (withDefault)

{- Possibly unify all of these in a `ListLike` class.

TODO: Should be able to unify `map`, unless the `Ord` constraint is a problem.
-}
import Elm.Set hiding
    ( union, toList, fromList, size, singleton, member, isEmpty, empty, remove
    , foldl, intersect, diff, insert, filter, partition, map
    )

import Elm.String hiding
    -- Perhaps these could be unified with ListLike
    ( length, isEmpty, reverse, repeat, concat, slice
    -- These are genuinely different, because `String` is not a `List Char`
    , map, filter, foldl, foldr, any, all, toList, fromList
    -- Perhaps LikeInt53
    , toInt
    -- Won't unify with Elm.Basics
    , toFloat
    )

-- Conflicts with Elm.Date
import Elm.Time hiding (toTime, fromTime, millisecond, second, minute, hour)
