
-- | Elm modules define `foldl` with a different signature than Purescript's.
-- | So, we define that alternative `foldl` here.

module Elm.Foldable (foldl) where


import Data.Foldable (class Foldable)
import Prelude (flip)


-- | Reduce a container from the left.
-- | 
-- |    foldl (:) [] [1,2,3] == [3,2,1]
foldl :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
foldl func = 
    Data.Foldable.foldl (flip func)
