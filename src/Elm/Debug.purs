
-- | This library is for investigating bugs or performance problems.

module Elm.Debug
    ( log, crash
    ) where

-- TODO: Is there anything sensible to do with `watch`, `watchSummary` and `trace`?


import Debug.Trace (trace, spy)
import Prelude ((++))
import Partial.Unsafe (unsafeCrashWith)


-- | Log a tagged value on the developer console, and then return the value.
-- | 
-- |     1 + log "number" 1        -- equals 2, logs "number: 1"
-- |     length (log "start" [])   -- equals 0, logs "start: []"
-- | 
-- | Notice that `log` is not a pure function! It should *only* be used for
-- | investigating bugs or performance problems.
log :: forall a. String -> a -> a
log tag value =
    trace (tag ++ ": ") (\_ -> spy value)


-- | Crash the program with an error message.
-- |
-- | Equivalent to Purescript's `Partial.Unsafe.unsafeCrashWith`
crash :: forall a. String -> a
crash = unsafeCrashWith
