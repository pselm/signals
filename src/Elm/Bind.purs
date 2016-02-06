module Elm.Bind
    ( andThen
    ) where

{-| Elm classes typically use `andThen` instead of `bind`. 
We define the synomyn here generically so that we don't have to define the
function multiple times. And, we can re-export it (as Elm code expects)
without producing conflicts, since it's all the same function.
-}

import Prelude (class Bind, bind)


-- | `andThen` is an alias for `bind`.
andThen :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
andThen = bind
