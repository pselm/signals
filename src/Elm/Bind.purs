
-- | Elm modules typically use `andThen` for what Purescript would call `bind`. 
-- | We define the synomyn here generically so that we don't have to define the
-- | function multiple times. And, we can re-export it (as Elm code expects)
-- |without producing conflicts, since it's all the same function.

module Elm.Bind (andThen) where


import Prelude (class Bind, bind)


-- | Given some computation, chain its result with another computation.
-- |
-- | `andThen` is equivalent to Purescript's `bind`.
andThen :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
andThen = bind
