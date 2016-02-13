
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
    , list, array
    , object
    ) where


import Data.Foreign (Foreign, toForeign)
import Elm.List (List)
import Data.Tuple (Tuple)
import Elm.Basics (Float, Bool)
import Data.Foldable (class Foldable, foldl)
import Prelude ((<<<), flip)
import Data.Array (cons, reverse)


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


-- | Encode a JSON object.
-- |
-- | The signature uses `Foldable` in order to work with `List` or
-- | `Array`, amongst others.
object :: forall f. (Foldable f) => f (Tuple String Value) -> Value
object = toForeign <<< Data.StrMap.fromFoldable 


-- | Encode Purescript's primitive `Array` type (distinct from Elm's `Array`).
psArray :: Array Value -> Value
psArray = toForeign


-- | Encode an array type. Uses a polymorphic type in order to accommodate
-- | Purescript `Array` and `Elm.Array`, among others.
array :: forall f. (Foldable f) => f Value -> Value
array = psArray <<< reverse <<< foldl (flip cons) []


-- | Encode a `List`.
list :: List Value -> Value
list = psArray <<< Data.List.toUnfoldable
