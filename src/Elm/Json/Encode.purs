module Elm.Json.Encode
    ( Value
    , encode
    , string, int, float, bool, null
    , list, array, jsArray
    , object
    ) where


{-| Library for turning Elm values into Json values.

-}


import Data.Foreign (Foreign, toForeign)
import Data.List (List)
import Data.Tuple (Tuple, fst, snd)
import Elm.Basics (Float)
import Prelude ((<<<))
import Elm.Array as ElmArray


{-| Represents a JavaScript value. -}
type Value = Foreign


{-| Convert a `Value` into a prettified string. The first argument specifies
the amount of indentation in the resulting string.

    person =
        object
          [ ("name", string "Tom")
          , ("age", int 42)
          ]

    compact = encode 0 person
    -- {"name":"Tom","age":42}

    readable = encode 4 person
    -- {
    --     "name": "Tom",
    --     "age": 42
    -- }
-}
foreign import encode :: Int -> Value -> String


{-|-}
string :: String -> Value
string = toForeign


{-|-}
int :: Int -> Value
int = toForeign


{-| Encode a Float. `Infinity` and `NaN` are encoded as `null`.
-}
float :: Float -> Value
float = toForeign


{-|-}
bool :: Boolean -> Value
bool = toForeign


{-|-}
null :: Value
null = encodeNull

foreign import encodeNull :: Value


{-|-}
object :: List (Tuple String Value) -> Value
object = encodeObject fst snd <<< Data.List.toUnfoldable


foreign import encodeObject ::
    (forall a b. Tuple a b -> a) ->
    (forall a b. Tuple a b -> b) ->
    Array (Tuple String Value) -> Value


jsArray :: Array Value -> Value
jsArray = toForeign


{-|-}
array :: ElmArray.Array Value -> Value
array = jsArray <<< Data.Sequence.toUnfoldable


{-|-}
list :: List Value -> Value
list = jsArray <<< Data.List.toUnfoldable
