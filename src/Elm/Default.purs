module Elm.Default
    ( module Virtual
    ) where

-- | Re-export the things which Elm imports by default.
-- |
-- | So, if you want the Elm default imports, you can do
-- |
-- | `import Elm.Default`

import Elm.Basics as Virtual
import Elm.Debug as Virtual
import Elm.List (List(), (:)) as Virtual
import Data.Maybe (Maybe (Just, Nothing)) as Virtual
import Elm.Result (Result (Ok, Err)) as Virtual
-- import Signal (Signal ()) as Virtual


