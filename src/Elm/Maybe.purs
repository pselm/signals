module Elm.Maybe 
    ( module Virtual
    , withDefault, oneOf, andThen
    , map, map2, map3, map4, map5
    ) where


{-| Additional functions are available in `Data.Maybe` -}


-- For re-export

import Data.Maybe (Maybe(..)) as Virtual


-- Internal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..))
import Control.Apply
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


{-| Transform a `Maybe` value with a given function:

    map sqrt (Just 9) == Just 3
    map sqrt Nothing == Nothing
-}
map :: forall a b. (a -> b) -> Maybe a -> Maybe b
map = Prelude.map 


{-| Apply a function if all the arguments are `Just` a value.

    map2 (+) (Just 3) (Just 4) == Just 7
    map2 (+) (Just 3) Nothing == Nothing
    map2 (+) Nothing (Just 4) == Nothing
-}
map2 :: forall a b value. (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 = lift2


map3 :: forall a b c value. (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 = lift3


map4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 = lift4


map5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
map5 = lift5


{-| Chain together many computations that may fail. It is helpful to see its
definition:

    andThen : Maybe a -> (a -> Maybe b) -> Maybe b
    andThen maybe callback =
        case maybe of
            Just value ->
                callback value

            Nothing ->
                Nothing

This means we only continue with the callback if things are going well. For
example, say you need to use (`head : List Int -> Maybe Int`) to get the
first month from a `List` and then make sure it is between 1 and 12:

    toValidMonth : Int -> Maybe Int
    toValidMonth month =
        if month >= 1 && month <= 12 then
            Just month
        else
            Nothing

    getFirstMonth : List Int -> Maybe Int
    getFirstMonth months =
        head months `andThen` toValidMonth

If `head` fails and results in `Nothing` (because the `List` was empty`),
this entire chain of operations will short-circuit and result in `Nothing`.
If `toValidMonth` results in `Nothing`, again the chain of computations
will result in `Nothing`.
-}
andThen :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
andThen = Prelude.bind
