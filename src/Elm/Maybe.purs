module Elm.Maybe 
    ( module Virtual
    , withDefault, oneOf 
    ) where


{-| Additional functions are available in `Data.Maybe` -}


-- For re-export

import Data.Maybe (Maybe(..)) as Virtual
import Prelude (map) as Virtual
import Elm.Apply (map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual

-- Internal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..))
import Prelude ()


{-| Provide a default value, turning an optional value into a normal
value.  This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.

    withDefault 100 (Just 42)   -- 42
    withDefault 100 Nothing     -- 100

    withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"

-}
withDefault :: forall a. a -> Maybe a -> a
withDefault = fromMaybe


{-| Pick the first `Maybe` that actually has a value. Useful when you want to
try a couple different things, but there is no default value.

    oneOf [ Nothing, Just 42, Just 71 ] == Just 42
    oneOf [ Nothing, Nothing, Just 71 ] == Just 71
    oneOf [ Nothing, Nothing, Nothing ] == Nothing
-}
oneOf :: forall a. List (Maybe a) -> Maybe a
oneOf maybes =
    case maybes of
        Nil ->
            Nothing

        Cons maybe rest ->
            case maybe of
                Nothing -> oneOf rest
                Just _ -> maybe

