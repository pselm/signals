module Test.UnqualifiedImport where

{- This is a test to see how much of the other modules can be imported
unqualified. Not that this is recommended, but it is nice to be able to
support it as much as is reasonable.
-}

-- Start with the imports which Elm does automatically
import Elm.Default

-- Then do the Elm-ish typeclasses
import Elm.Apply
import Elm.Bind

-- Then Elm.Basics
import Elm.Basics

-- Then, the rest in alphabetical order

-- `Array` conflicts with `Prim.Array`
import Elm.Array hiding (Array)

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

-- oneOf conflicts with Elm.Maybe ... perhaps could be unified?
import Elm.Json.Decode hiding (oneOf)

-- Inevitably conflicts with Elm.Json.Decode ... one will usually need to import
-- these qualified.
import Elm.Json.Encode hiding (string, int, float, bool, null, list, array)

{- There are a number of things that could conceivably be unified between
`Elm.List` and `Elm.Array` ... `length`, `filter`, `indexedMap`,
`repeat`

It should be possible to unify `isEmpty`, `member` as between `Dict` and `List`.
-}
import Elm.List hiding
    ( length, filter, isEmpty, member, indexedMap, repeat
    -- The mapping functions are not the normal apply in List
    , map2, map3, map4, map5
    )

import Elm.Maybe

-- Conflicts with Elm.Json.Decode and Elm.Json.Encode. I don't suppose there's
-- a sensible type-class to be had here?
import Elm.Random hiding (bool, int, float, list)

-- Conflicts with String
import Elm.Regex hiding (contains, split)

{- Possibly unify `withDefault` with `Maybe` -}
import Elm.Result hiding (withDefault)

{- Possibly unify all of these in a `ListLike` class.

TODO: Should be able to unify `map`, unless the `Ord` constraint is a problem.
-}
import Elm.Set hiding
    ( union, toList, fromList, size, singleton, member, isEmpty, empty, remove
    , intersect, diff, insert, filter, partition, map
    )

-- Signal's map is a monadic map, so not part of the type class 
import Elm.Signal hiding (map, map2, map3, map4, map5, filter, filterMap)

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

-- Should make succeed a synonym for pure everywhere
-- Conflicts with Elm.Json.Decode.
-- Not sure what to do about `fail`
-- `toMaybe` and `fromMaybe` conflict with Elm.Result ... is a type-class sensible?
import Elm.Task hiding (succeed, fail, toMaybe, fromMaybe)

-- Conflicts with Elm.Date
import Elm.Time hiding (toTime, fromTime, millisecond, second, minute, hour)
