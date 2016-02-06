
-- | A library for turning Elm values into Json values.
-- |
-- | This is mainly implemented via the `toForeign` method in
-- | [purescript-foreign](https://pursuit.purescript.org/packages/purescript-foreign/0.7.2/docs/Data.Foreign#v:toForeign).
-- |
-- | You could also consider the purescript-argonaut-* modules.

module Elm.Json.Encode
    ( Value
    , encode
    , string, int, float, bool, null
    , list, array, psArray
    , object
    ) where


import Data.Foreign (Foreign, toForeign)
import Data.List (List)
import Data.Tuple (Tuple, fst, snd)
import Elm.Basics (Float, Bool)
import Prelude ((<<<))
import Elm.Array as ElmArray


-- | Represents a JavaScript value.
type Value = Foreign


-- | Convert a `Value` into a prettified string. The first argument specifies
-- | the amount of indentation in the resulting string.
-- | 
-- |     person =
-- |         object
-- |           [ Tuple "name" (string "Tom")
-- |           , Tuple "age" (int 42)
-- |           ]
-- | 
-- |     compact = encode 0 person
-- |     -- {"name":"Tom","age":42}
-- | 
-- |     readable = encode 4 person
-- |     -- {
-- |     --     "name": "Tom",
-- |     --     "age": 42
-- |     -- }
foreign import encode :: Int -> Value -> String


-- | Turn a `String` into a `Value`.
string :: String -> Value
string = toForeign


-- | Turn an `Int` into a `Value`.
int :: Int -> Value
int = toForeign


-- | Encode a Float. `Infinity` and `NaN` are encoded as `null`.
float :: Float -> Value
float = toForeign


-- | Encode a `Bool`.
bool :: Bool -> Value
bool = toForeign


-- | Encode a null value.
null :: Value
null = encodeNull

foreign import encodeNull :: Value


-- TODO: I should make this work with any `Foldable`, so it will work with Array or List

-- | Encode a JSON object.
object :: List (Tuple String Value) -> Value
object = encodeObject fst snd <<< Data.List.toUnfoldable


foreign import encodeObject ::
    (forall a b. Tuple a b -> a) ->
    (forall a b. Tuple a b -> b) ->
    Array (Tuple String Value) -> Value


-- | Encode Purescript's primitive `Array` type (distinct from Elm's `Array`).
psArray :: Array Value -> Value
psArray = toForeign


-- | Encode Elm's `Array` type.
array :: ElmArray.Array Value -> Value
array = psArray <<< Data.Sequence.toUnfoldable


-- | Encode a `List`.
list :: List Value -> Value
list = psArray <<< Data.List.toUnfoldable
