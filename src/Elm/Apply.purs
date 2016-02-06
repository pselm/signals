
-- | Elm modules typically use `map2` through `map5` for what Purescript's
-- | `Apply` class would call `lift2` through `lift5`.
-- |
-- | So, we define `map2` through `map5` here as synonyms for `lift2` through
-- | `lift5`. We also re-export these in the individual Elm modules that use
-- | them, so that that the API matches with the Elm API. By re-exporting
-- | functions, we gain a (slight) efficiency over defining them again.
-- |
-- | We also make `andMap` a synonym for Purescript's `apply`.

module Elm.Apply
    ( andMap
    , map2, map3, map4, map5
    ) where


import Control.Apply (lift2, lift3, lift4, lift5)
import Prelude (class Apply, apply)


-- | Map a function of two arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift2`.
map2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
map2 = lift2


-- | Map a function of three arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift3`.
map3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
map3 = lift3


-- | Map a function of four arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift4`.
map4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
map4 = lift4


-- | Map a function of five arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift5`.
map5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
map5 = lift5


-- | Map a function in a container to a value in a container.
-- |
-- | This is the equivalent of Purescript's `apply`.
andMap :: forall a b f. (Apply f) => f (a -> b) -> f a -> f b
andMap = apply
