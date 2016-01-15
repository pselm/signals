module Elm.Apply
    ( map2
    , map3
    , map4
    , map5
    ) where

{-| Elm classes typically use map2 through map5 instead of
lift2 through lift5. We define those synomyns here generically
so that we don't have to define the functions multiple times.
-}

import Control.Apply
import Prelude


-- | Lift a function of two arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
map2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
map2 = lift2

-- | Lift a function of three arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
map3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
map3 = lift3

-- | Lift a function of four arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
map4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
map4 = lift4

-- | Lift a function of five arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
map5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
map5 = lift5
