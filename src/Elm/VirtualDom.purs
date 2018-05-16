
-- | > API to the core diffing algorithm. Can serve as a foundation for libraries
-- | > that expose more helper functions for HTML or SVG.

module Elm.VirtualDom
    ( module Virtual
    , Node
    , text, node
    , Property, property, attribute, attributeNS, mapProperty
    , style
    , on, onWithOptions, Options, defaultOptions, equalOptions
    , lazy, lazy_, lazy2, lazy2_, lazy3, lazy3_
    , keyedNode
    , fromRenderable
    , program, programWithFlags
    ) where


import Control.Apply (lift2)
import Control.Comonad (extract)
import Control.Monad (when, unless, (>=>))
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, forE, runPure, kind Effect)
import Control.Monad.Eff.AVar (putVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT, throwError)
import Control.Monad.IO (IO)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Control.Monad.ST (pureST, newSTRef, writeSTRef, readSTRef)
import DOM (DOM)
import DOM.Event.Event (currentTarget, preventDefault, stopPropagation)
import DOM.Event.EventTarget (addEventListener, dispatchEvent, eventListener)
import DOM.Event.Types (Event, EventTarget, EventType, customEventToEvent)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToEventTarget, htmlElementToNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (setAttribute, removeAttribute)
import DOM.Node.Node (appendChild, parentNode, replaceChild, lastChild, setTextContent, removeChild, childNodes, nextSibling, previousSibling)
import DOM.Node.NodeList (item)
import DOM.Node.Types (Document, Element, elementToEventTarget, elementToNode, textToNode)
import DOM.Node.Types (Node) as DOM
import DOM.Renderable (class Renderable, AnyRenderable, EffDOM, toAnyRenderable)
import DOM.Renderable (render, updateDOM) as Renderable
import Data.Array (null) as Array
import Data.Array.ST (runSTArray, emptySTArray, pushSTArray)
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
import Data.Either (Either(..), either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldl, for_)
import Data.Foreign (ForeignError(..), readString, toForeign)
import Data.Lazy (Lazy, defer, force)
import Data.Leibniz (type (~), Leibniz(..))
import Data.List (List(..), length, reverse, singleton, snoc, drop, zip)
import Data.List (foldM, null) as List
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Ord (abs, max)
import Data.StrMap (StrMap, foldM, foldMap, lookup, isEmpty)
import Data.StrMap (delete, empty, insert, lookup) as StrMap
import Data.StrMap.ST (new, poke, peek)
import Data.StrMap.ST.Unsafe (unsafeFreeze)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\), type (/\))
import Elm.Basics (Bool)
import Elm.Graphics.Internal (EventHandler, addEventHandler, detail, documentForNode, eventHandler, eventToCustomEvent, makeCustomEvent, nextEventTarget, nodeToElement, removeAttributeNS, removeEventHandler, removeProperty, removeStyle, setAttributeNS, setHandlerInfo, setProperty, setPropertyIfDifferent, setStyle)
import Elm.Json.Decode (Decoder, Value, decodeValue, equalDecodersL)
import Elm.Platform (Cmd, Program, Sub)
import Elm.Result (Result(..))
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Eq, class Functor, class Show, Unit, bind, const, discard, eq, flip, id, map, not, pure, show, unit, void, (#), ($), (&&), (*), (+), (-), (/=), (<), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=), (||))
import Prelude (map) as Virtual
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq, unsafeRefEq)


-- Will suggest these for Data.Exists if they work
foreign import data Exists2 :: (Type -> Type -> Type) -> Type

mkExists2 :: ∀ f a b. f a b -> Exists2 f
mkExists2 = unsafeCoerce

runExists2 :: ∀ f r. (∀ a b. f a b -> r) -> Exists2 f -> r
runExists2 = unsafeCoerce


foreign import data Exists3 :: (Type -> Type -> Type -> Type) -> Type

mkExists3 :: ∀ f a b c. f a b c -> Exists3 f
mkExists3 = unsafeCoerce

runExists3 :: ∀ f r. (∀ a b c. f a b c -> r) -> Exists3 f -> r
runExists3 = unsafeCoerce


-- | > An immutable chunk of data representing a DOM node. This can be HTML or SVG.
data Node msg
    = Text String
    | PlainNode (NodeRecord msg) (List (Node msg))
    | KeyedNode (NodeRecord msg) (List (Tuple String (Node msg)))
    | Tagger (Coyoneda Node msg)
    | Thunk (Lazy (Node msg)) (Thunk msg)
    | Thunk2 (Lazy (Node msg)) (Thunk2 msg)
    | Thunk3 (Lazy (Node msg)) (Thunk3 msg)
    | Custom AnyRenderable


instance functorNode :: Functor Node where
    map func child = Tagger $ coyoneda func child


type NodeRecord msg =
    { tag :: String
    , namespace :: Maybe String
    , facts :: OrganizedFacts msg
    }


-- Note that something will need to be listening for the Elm `msg` custom
-- events and send them back into an event loop for this to be fully
-- functional.
instance renderableNode :: Renderable (Node msg) where
    render = render

    update old current =
        applyPatches old.result $
            diff old.value current


-- Try to determine whether two taggers are equal ... as best we can. We return
-- `Nothing` in cases where we can't be sure whether they are equal or not, given
-- the limits of reference equality for functions. Note that we don't check the
-- final subnode ... this just checks the taggers.
equalTaggers :: ∀ msg1 msg2. Coyoneda Node msg1 -> Coyoneda Node msg2 -> Maybe Bool
equalTaggers coyo1 coyo2 =
    coyo1 # unCoyoneda \func1 subNode1 ->
    coyo2 # unCoyoneda \func2 subNode2 ->
        case subNode1, subNode2 of
            Tagger subcoyo1, Tagger subcoyo2 ->
                -- They both have another layer of tagger, so check reference
                -- equality and recurse. (The recursion may give us a `Just`
                -- or a `Nothing`, so we carry that through).
                equalTaggers subcoyo1 subcoyo2
                    <#> (&&) (reallyUnsafeRefEq func1 func2)

            Tagger subcoyo1, _ ->
                -- There's an extra tagger on the left, so we're definitely
                -- not equal
                Just false

            _, Tagger subcoyo2 ->
                -- There's an extra tagger on the right, so we're definitely
                -- not equal
                Just false

            _, _ ->
                -- We've reached the end of the taggers on both sides, so we're
                -- either definitely equal or we can't tell.
                if reallyUnsafeRefEq func1 func2 then
                    Just true
                else
                    Nothing


type Thunk msg = Exists (ThunkRecord1 msg)
type Thunk2 msg = Exists2 (ThunkRecord2 msg)
type Thunk3 msg = Exists3 (ThunkRecord3 msg)


newtype ThunkRecord1 msg a = ThunkRecord1
    { func :: a -> Node msg
    , arg :: a /\ Maybe (a -> a -> Bool)
    }


newtype ThunkRecord2 msg a b = ThunkRecord2
    { func :: a -> b -> Node msg
    , arg1 :: a /\ Maybe (a -> a -> Bool)
    , arg2 :: b /\ Maybe (b -> b -> Bool)
    }


newtype ThunkRecord3 msg a b c = ThunkRecord3
    { func :: a -> b -> c -> Node msg
    , arg1 :: a /\ Maybe (a -> a -> Bool)
    , arg2 :: b /\ Maybe (b -> b -> Bool)
    , arg3 :: c /\ Maybe (c -> c -> Bool)
    }


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks :: ∀ a b. Thunk a -> Thunk b -> Bool
equalThunks left right =
    left # runExists (\(ThunkRecord1 a) ->
    right # runExists (\(ThunkRecord1 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg (unsafeCoerce b.arg)
    ))


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks2 :: ∀ a b. Thunk2 a -> Thunk2 b -> Bool
equalThunks2 left right =
    left # runExists2 (\(ThunkRecord2 a) ->
    right # runExists2 (\(ThunkRecord2 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg1 (unsafeCoerce b.arg1) &&
        equalArgs a.arg2 (unsafeCoerce b.arg2)
    ))


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks3 :: ∀ a b. Thunk3 a -> Thunk3 b -> Bool
equalThunks3 left right =
    left # runExists3 (\(ThunkRecord3 a) ->
    right # runExists3 (\(ThunkRecord3 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg1 (unsafeCoerce b.arg1) &&
        equalArgs a.arg2 (unsafeCoerce b.arg2) &&
        equalArgs a.arg3 (unsafeCoerce b.arg3)
    ))


-- | Checks possibly equality where we may have captured an `Eq` instance or
-- | may not. Assumes that we've already coerced the types if we have
-- | sufficient evidence to have done that.
equalArgs :: ∀ a. Tuple a (Maybe (a -> a -> Bool)) -> Tuple a (Maybe (a -> a -> Bool)) -> Bool
equalArgs (Tuple left leftEq) (Tuple right rightEq) =
    case left, leftEq, right, rightEq of
        a, Just equals, b, _ ->
            unsafeRefEq a b || equals a b

        a, _, b, Just equals ->
            unsafeRefEq a b || equals a b

        a, _, b, _ ->
            unsafeRefEq a b


-- | > Create a DOM node with a tag name, a list of HTML properties that can
-- | > include styles and event listeners, a list of CSS properties like `color`, and
-- | > a list of child nodes.
-- | >
-- | >     import Json.Encode as Json
-- | >
-- | >     hello :: Node msg
-- | >     hello =
-- | >       node "div" [] [ text "Hello!" ]
-- | >
-- | >     greeting :: Node msg
-- | >     greeting =
-- | >       node "div"
-- | >         [ property "id" (Json.string "greeting") ]
-- | >         [ text "Hello!" ]
node :: ∀ msg. String -> List (Property msg) -> List (Node msg) -> Node msg
node tag properties =
    PlainNode
        { tag
        , namespace: organized.namespace
        , facts: organized.facts
        }

    where
        organized =
            organizeFacts properties


-- | > Works just like `node`, but you add a unique identifier to each child
-- | > node. You want this when you have a list of nodes that is changing: adding
-- | > nodes, removing nodes, etc. In these cases, the unique identifiers help make
-- | > the DOM modifications more efficient.
keyedNode :: ∀ msg. String -> List (Property msg) -> List (Tuple String (Node msg)) -> Node msg
keyedNode tag properties =
    KeyedNode
        { tag
        , namespace: organized.namespace
        , facts: organized.facts
        }

    where
        organized =
            organizeFacts properties


-- | Create a `Node` from anything that has a `Renderable` instance.
fromRenderable :: ∀ a msg. (Renderable a) => a -> Node msg
fromRenderable =
    -- The original Elm allows a list of facts here as well, but that ends up being
    -- incoherent, since if we change what the renderable produces, then the
    -- renderable can be out of sync for updates.
    --
    -- TODO: Revisit this for Elm 0.18's version ...
    Custom <<< toAnyRenderable


-- | > Just put plain text in the DOM. It will escape the string so that it appears
-- | > exactly as you specify.
-- | >
-- | >     text "Hello World!"
text :: ∀ msg. String -> Node msg
text = Text


-- PROPERTIES

-- | > When using HTML and JS, there are two ways to specify parts of a DOM node.
-- | >
-- | >   1. Attributes &mdash; You can set things in HTML itself. So the `class`
-- | >      in `<div class="greeting"></div>` is called an *attribute*.
-- | >
-- | >   2. Properties &mdash; You can also set things in JS. So the `className`
-- | >      in `div.className = 'greeting'` is called a *property*.
-- | >
-- | > So the `class` attribute corresponds to the `className` property. At first
-- | > glance, perhaps this distinction is defensible, but it gets much crazier.
-- | > *There is not always a one-to-one mapping between attributes and properties!*
-- | > Yes, that is a true fact. Sometimes an attribute exists, but there is no
-- | > corresponding property. Sometimes changing an attribute does not change the
-- | > underlying property. For example, as of this writing, the `webkit-playsinline`
-- | > attribute can be used in HTML, but there is no corresponding property!
data Property msg
    = CustomProperty Key Value
    | Attribute Key String
    | AttributeNS Namespace Key String
    | Styles (List (Tuple String String))
    | OnEvent Key Options (Decoder msg)


derive instance functorProperty :: Functor Property


-- | > Transform the messages produced by a `Property`.
-- |
-- | Equivalent to Purescript's `map`
mapProperty :: ∀ a b. (a -> b) -> Property a -> Property b
mapProperty = map



-- This represents the properties that should be applied to a node, organized for
-- quick lookup (since the API supplies them as a list).
type OrganizedFacts msg =
    { attributes :: StrMap String
    , attributesNS :: StrMap (StrMap String)
    , events :: StrMap (Tuple Options (Decoder msg))
    , styles :: StrMap String
    , properties :: StrMap Value
    }


data FactChange msg
    = AddAttribute String String
    | RemoveAttribute String
    | AddAttributeNS String String String
    | RemoveAttributeNS String String
    | AddEvent EventType Options (Decoder msg)
    | MutateEvent EventType Options (Decoder msg)
    | RemoveEvent EventType Options
    | AddStyle String String
    | RemoveStyle String
    | RemoveAllStyles
    | AddProperty String Value
    | RemoveProperty String


type Namespace = String
type Key = String


-- Basically, this seems to take a list of properties and group
-- them according to their subtypes. Also, if properties of the
-- same subtype have the same key, it would only retain one of
-- the properties.
--
-- One alternative would be a foldl, with the OrganizedFacts as
-- the memo. But that might be more inefficient. I should
-- profile the overall code sometime and see where optimizations
-- might be wise.
organizeFacts :: ∀ msg. List (Property msg) -> {namespace :: Maybe String, facts :: OrganizedFacts msg}
organizeFacts factList =
    pureST do
        -- Create a bunch of accumulators for StrMap
        mutableAttributes <- new
        mutableAttributesNS <- new
        mutableEvents <- new
        mutableStyles <- new
        mutableProperties <- new

        -- And a reference for the namespace
        mutableNamespace <- newSTRef Nothing

        -- Iterate through the facts
        for_ factList \fact -> do
            case fact of
                Attribute key value ->
                    void $ poke mutableAttributes key value

                -- We make mutableAttributesNS a map of maps, where the outer map
                -- is keyed by the namespace. This fixes a bug in the original
                -- Elm Javascript ... see https://github.com/elm-lang/virtual-dom/issues/16
                AttributeNS ns key value -> void do
                    submap <-
                        peek mutableAttributesNS ns >>=
                            case _ of
                                Just existing ->
                                    pure existing

                                Nothing ->
                                    new

                    void $ poke submap key value
                    void $ poke mutableAttributesNS ns submap

                OnEvent key options decoder ->
                    void $ poke mutableEvents key (Tuple options decoder)

                Styles list ->
                    for_ list \(Tuple key value) ->
                        void $ poke mutableStyles key value

                CustomProperty key value ->
                    -- So, the normal case here is that we're setting an arbitrary property
                    -- on the node. However, the original Elm code also allows you to use
                    -- a special "namespace" property, to specify the namespace of the node
                    -- itself. This seems a bit odd ... would probably be better to have an
                    -- explicit API for namespaced nodes.
                    if key == "namespace" then
                        case extract $ runExceptT $ readString value of
                            Left _ ->
                                -- It wasn't a string, so don't handle specially
                                void $ poke mutableProperties key value

                            Right s ->
                                -- It was a string, so store as the namespace
                                void $ writeSTRef mutableNamespace (Just s)

                    else if key == "className" then do
                        -- The Elm code also special-cases `className` so that
                        -- if you supply it multiple times, it will be
                        -- additive, rather than clobbering the previous one.
                        oldString <-
                            peek mutableProperties key
                            <#> maybe (throwError $ pure $ ErrorAtProperty "className" $ JSONError "missing") readString

                        let newString = readString value
                        let combined = lift2 (\a b -> toForeign $ a <> " " <> b) oldString newString

                        -- If we've had any kind of error above, we just use the raw value
                        void $ poke mutableProperties key $ either (const value) id $ runExcept combined

                    else
                        -- This is the general case.
                        void $ poke mutableProperties key value

        -- These are unsafe in the sense that further modifications to the mutable
        -- versions would also modify the pure. So, we won't do that ...
        -- The alternative is freezeST, but that actually does a copy, which in
        -- this context isn't really necessary.
        attributes <- unsafeFreeze mutableAttributes
        events <- unsafeFreeze mutableEvents
        styles <- unsafeFreeze mutableStyles
        properties <- unsafeFreeze mutableProperties

        -- I also need to iterate over all of the submaps and "freeze" them ...
        -- and then freeze the resulting outer map
        pureOuterMap <- unsafeFreeze mutableAttributesNS
        accumulator <- new

        void $ foldM (\accum key submap ->
            unsafeFreeze submap >>= poke accum key
        ) accumulator pureOuterMap

        attributesNS <- unsafeFreeze accumulator

        -- And read the namespace
        namespace <- readSTRef mutableNamespace

        pure
            { namespace
            , facts: { attributes, attributesNS, events, styles, properties }
            }


-- | > Create arbitrary *properties*.
-- | >
-- | >     import JavaScript.Encode as Json
-- | >
-- | >     greeting : Html
-- | >     greeting =
-- | >         node "div" [ property "className" (Json.string "greeting") ] [
-- | >           text "Hello!"
-- | >         ]
-- | >
-- | > Notice that you must give the *property* name, so we use `className` as it
-- | > would be in JavaScript, not `class` as it would appear in HTML.
property :: ∀ msg. String -> Value -> Property msg
property = CustomProperty


-- | > Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
-- | > function under the hood.
-- | >
-- | >     greeting : Html
-- | >     greeting =
-- | >         node "div" [ attribute "class" "greeting" ] [
-- | >           text "Hello!"
-- | >         ]
-- | >
-- | > Notice that you must give the *attribute* name, so we use `class` as it would
-- | > be in HTML, not `className` as it would appear in JS.
attribute :: ∀ msg. String -> String -> Property msg
attribute = Attribute


-- | > Would you believe that there is another way to do this?! This corresponds
-- | > to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
-- | > much the same thing as `attribute` but you are able to have "namespaced"
-- | > attributes. This is used in some SVG stuff at least.
-- |
-- | Note that the first argument is the namespace, the second the label, and
-- | third the value.
attributeNS :: ∀ msg. String -> String -> String -> Property msg
attributeNS = AttributeNS


-- | > Specify a list of styles.
-- | >
-- | >     myStyle :: Property msg
-- | >     myStyle =
-- | >       style
-- | >         [ Tuple "backgroundColor" "red"
-- | >         , Tuple "height" "90px"
-- | >         , Tuple "width" "100%"
-- | >         ]
-- | >
-- | >     greeting :: Node msg
-- | >     greeting =
-- | >       node "div" [ myStyle ] [ text "Hello!" ]
style :: ∀ msg. List (Tuple String String) -> Property msg
style = Styles


-- EVENTS

-- | > Create a custom event listener.
-- | >
-- | >     import Json.Decode as Json
-- | >
-- | >     onClick : msg -> Property msg
-- | >     onClick msg =
-- | >       on "click" (Json.succeed msg)
-- | >
-- | > You first specify the name of the event in the same format as with JavaScript’s
-- | > `addEventListener`. Next you give a JSON decoder, which lets you pull
-- | > information out of the event object. If the decoder succeeds, it will produce
-- | > a message and route it to your `update` function.
on :: ∀ msg. String -> Decoder msg -> Property msg
on = flip onWithOptions defaultOptions


-- | > Same as `on` but you can set a few options.
onWithOptions :: ∀ msg. String -> Options -> Decoder msg -> Property msg
onWithOptions = OnEvent


-- | > Options for an event listener. If `stopPropagation` is true, it means the
-- | > event stops traveling through the DOM so it will not trigger any other event
-- | > listeners. If `preventDefault` is true, any built-in browser behavior related
-- | > to the event is prevented. For example, this is used with touch events when you
-- | > want to treat them as gestures of your own, not as scrolls.
type Options =
    { stopPropagation :: Bool
    , preventDefault :: Bool
    }


-- | > Everything is `False` by default.
-- | >
-- | >     defaultOptions =
-- | >         { stopPropagation = False
-- | >         , preventDefault = False
-- | >         }
defaultOptions :: Options
defaultOptions =
    { stopPropagation: false
    , preventDefault: false
    }


-- | Elm doesn't need this function because its `==` can handle record types
-- | magically. This isn't possible in Purescript without a newtype, which we'd
-- | like to avoid here. So, we define a custom equality function.
equalOptions :: Options -> Options -> Bool
equalOptions a b =
    a.stopPropagation == b.stopPropagation &&
    a.preventDefault == b.preventDefault


-- OPTIMIZATION

-- | > A performance optimization that delays the building of virtual DOM nodes.
-- | >
-- | > Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
-- | > it. Calling `(lazy view model)` delays the call until later. During diffing, we
-- | > can check to see if `model` is referentially equal to the previous value used,
-- | > and if so, we just stop. No need to build up the tree structure and diff it,
-- | > we know if the input to `view` is the same, the output must be the same!
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
-- |
-- | For a version of this function that doesn't require an `Eq` instance, see
-- | `lazy_`. This one will do a better job of detecting equality.
--
-- TODO: One idea I coul explore is an `Eq`-like instance that represents
-- something better than `unsafeRefEq` but not as reliable as `Eq` ... that is,
-- no false positives, but some false negatives. Perhaps `PartialEq`? Though
-- `Partial` doesn't quite have the right connotation.
lazy :: ∀ a msg. Eq a => (a -> Node msg) -> a -> Node msg
lazy func arg =
    Thunk (defer \_ -> func arg) $ mkExists $ ThunkRecord1
        { func
        , arg : arg /\ Just eq
        }


-- | Like `lazy`, but does not require an `Eq` instance. Using `lazy` will do
-- | a better job of detecting equality.
--
-- In Purescript 0.12, I should be able to pick up a possibly-existing `Eq`
-- instance with instance chains, without needing a separate function. (I could
-- do it now with overlapping instances, but may as well wait).
lazy_ :: ∀ a msg. (a -> Node msg) -> a -> Node msg
lazy_ func arg =
    Thunk (defer \_ -> func arg) $ mkExists $ ThunkRecord1
        { func
        , arg : arg /\ Nothing
        }


-- | > Same as `lazy` but checks on two arguments.
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
lazy2 :: ∀ a b msg. Eq a => Eq b => (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 func arg1 arg2 =
    Thunk2 (defer \_ -> func arg1 arg2) $ mkExists2 $ ThunkRecord2
        { func
        , arg1 : arg1 /\ Just eq
        , arg2 : arg2 /\ Just eq
        }


-- | Like `lazy2`, but does not require an `Eq` instance. Using `lazy2` will do
-- | a better job of detecting equality.
lazy2_ :: ∀ a b msg. (a -> b -> Node msg) -> a -> b -> Node msg
lazy2_ func arg1 arg2 =
    Thunk2 (defer \_ -> func arg1 arg2) $ mkExists2 $ ThunkRecord2
        { func
        , arg1 : arg1 /\ Nothing
        , arg2 : arg2 /\ Nothing
        }


-- | > Same as `lazy` but checks on three arguments.
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
lazy3 :: ∀ a b c msg. Eq a => Eq b => Eq c => (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 func arg1 arg2 arg3 =
    Thunk3 (defer \_ -> func arg1 arg2 arg3) $ mkExists3 $ ThunkRecord3
        { func
        , arg1 : arg1 /\ Just eq
        , arg2 : arg2 /\ Just eq
        , arg3 : arg3 /\ Just eq
        }


-- | Like `lazy3`, but does not require an `Eq` instance. Using `lazy3` will do
-- | a better job of detecting equality.
lazy3_ :: ∀ a b c msg. (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3_ func arg1 arg2 arg3 =
    Thunk3 (defer \_ -> func arg1 arg2 arg3) $ mkExists3 $ ThunkRecord3
        { func
        , arg1 : arg1 /\ Nothing
        , arg2 : arg2 /\ Nothing
        , arg3 : arg3 /\ Nothing
        }


-- RENDER


-- | If we have consecutive taggers, compose them together. We don't do this up
-- | front, because we want to be able to use `unsafeRefEq` on the functions,
-- | which won't work after we compose them. But it's handy sometimes, because
-- | we don't write out a distinct DOM node for each consecutive tagger.
composeTaggers :: ∀ msg. Coyoneda Node msg -> Coyoneda Node msg
composeTaggers coyo =
    coyo # unCoyoneda \func subNode ->
        case subNode of
            Tagger subcoyo ->
                composeTaggers (func <$> subcoyo)

            _ ->
                coyo


render :: ∀ e msg. Document -> Node msg -> EffDOM e DOM.Node
render doc vNode =
    case vNode of
        Thunk l _ ->
            render doc (force l)

        Thunk2 l _ ->
            render doc (force l)

        Thunk3 l _ ->
            render doc (force l)

        Text string ->
            createTextNode string doc
            <#> textToNode

        PlainNode rec children -> do
            domNode <-
                case rec.namespace of
                    Just ns ->
                        createElementNS rec.namespace rec.tag doc

                    Nothing ->
                        createElement rec.tag doc

            applyFacts (initialFactChanges rec.facts) domNode

            for_ children \child -> do
                renderedChild <- render doc child
                appendChild renderedChild (elementToNode domNode)

            pure (elementToNode domNode)

        KeyedNode rec children -> do
            domNode <-
                case rec.namespace of
                    Just ns ->
                        createElementNS rec.namespace rec.tag doc

                    Nothing ->
                        createElement rec.tag doc

            applyFacts (initialFactChanges rec.facts) domNode

            for_ children \(Tuple key child) -> do
                renderedChild <- render doc child
                appendChild renderedChild (elementToNode domNode)

            pure (elementToNode domNode)

        Tagger coyo ->
            -- We compose consecutive taggers now, rather than up-front,
            -- because we want to compare the funcs for reference equality
            -- when diffing. But it's better to compose here, because we're
            -- only writing out one DOM node. Remember to check how this
            -- affects traversals and indexes!
            composeTaggers coyo # unCoyoneda \func subNode -> do
                domNode <-
                    render doc subNode

                -- Now, this may be a flaw in my scheme. I can't add a listener
                -- to something unless it's an Element, not a Node. Now, in
                -- theory you can add a tagger in Elm to a Node that isn't an
                -- Element ...  e.g. a text node. However, I'm pretty sure that
                -- it doesn't matter, because you can't attach Elm listeners to
                -- plain text ...  you'd need to put it in a `span` or
                -- something. (Since plain text has no Elm attributes). So, I
                -- think this will work out OK, but we'll need to see.
                for_ (nodeToElement domNode) \element -> do
                    tagger <-
                        makeTagger func

                    addEventHandler elmMsgEvent tagger false (elementToEventTarget element)

                    -- We also store it as a property on the node, so that we can
                    -- mutate the handler in future instead of removing it and
                    -- re-adding it.
                    --
                    -- I suppose at some point I should reconsider this strategy of
                    -- leaving things in the DOM ... it's a handy way to keep state
                    -- around, I suppose, but we could probably do better if we
                    -- tried.
                    setTagger tagger element

                pure domNode

        Custom renderable ->
            Renderable.render doc renderable
            -- The original Elm code also tracks Facts and applies them here
            -- ...  in my structure, it seems best not to do that, since it's
            -- really the job of the renderable, but this may need to be
            -- revisited at some point.


-- APPLY FACTS

applyFacts :: ∀ e f msg. (Foldable f) => f (FactChange msg) -> Element -> EffDOM e Unit
applyFacts operations elem = do
    for_ operations \operation ->
        case operation of
            AddAttribute key value ->
                setAttribute key value elem

            RemoveAttribute key ->
                removeAttribute key elem

            AddAttributeNS ns key value ->
                setAttributeNS ns key value elem

            RemoveAttributeNS ns key ->
                removeAttributeNS ns key elem

            AddEvent key options decoder -> do
                handlers  <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                handler <-
                    makeEventHandler {options, decoder}

                let newHandlers = StrMap.insert (unwrap key) handler handlers

                addEventHandler key handler false (elementToEventTarget elem)
                setHandlers newHandlers elem

            MutateEvent key options decoder -> do
                handlers <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                let handler =
                        case StrMap.lookup (unwrap key) handlers of
                            Just h ->
                                h

                            Nothing ->
                                unsafeCrashWith "Could not find expected handler in DOM"

                setHandlerInfo {options, decoder} handler

            RemoveEvent key options -> do
                handlers <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                let handler =
                        case StrMap.lookup (unwrap key) handlers of
                            Just h ->
                                h

                            Nothing ->
                                unsafeCrashWith "Could not find expected handler in DOM"

                removeEventHandler key handler false (elementToEventTarget elem)

                let newHandlers = StrMap.delete (unwrap key) handlers
                setHandlers newHandlers elem

            AddStyle key value ->
                setStyle key value elem

            RemoveStyle key ->
                removeStyle key elem

            RemoveAllStyles ->
                removeAttribute "style" elem

            AddProperty key value ->
                if key == "value"
                    -- Changes to value are deferred to the real DOM, instead of the virtual
                    -- DOM, since the browser will change "value" behind our backs.
                    -- Note that we should probably treat "checked" like this as well:
                    -- https://github.com/elm-lang/virtual-dom/issues/117
                    then setPropertyIfDifferent key value elem
                    else setProperty key value elem

            RemoveProperty key ->
                removeProperty key elem


-- These store & retrieve some data as properties in the DOM ... should
-- re-assess that strategy at some point!
foreign import getHandlers :: ∀ e1 e2 msg. Element -> Eff e1 (Nullable (StrMap (MsgHandler e2 msg)))
foreign import setHandlers :: ∀ e1 e2 msg. StrMap (MsgHandler e1 msg) -> Element -> Eff e2 Unit
foreign import removeHandlers :: ∀ e. Element -> Eff e Unit

foreign import getTagger :: ∀ e a b. Element -> Eff e (Nullable (TaggerHandler e a b))
foreign import setTagger :: ∀ e1 e2 a b. TaggerHandler e1 a b -> Element -> Eff e2 Unit
foreign import removeTagger :: ∀ e. Element -> Eff e Unit


-- When Elm knows that there previously was a listener for an event, it does a
-- kind of "mutate listener" instead of removing the listener and adding a new
-- one. This might happen frequently, because testing the equality of decoders
-- is not fully decidable. So, you may end up with a lot of spurious listener
-- mutations. Removing and adding listeners is probably a little expensive, and
-- could even affect behaviour in some cases. So, instead, Elm mutates the
-- listener, by having the listener refer to some data "deposited" as a
-- property on the DOM node. Thus, we can mutate the listener by changing that
-- data.


-- | The information that a handler needs (in addition to the event!)
type HandlerInfo msg =
    { decoder :: Decoder msg
    , options :: Options
    }


type MsgHandler e msg =
    EventHandler (dom :: DOM, err :: EXCEPTION | e) (HandlerInfo msg)


-- | The name for the custom event we use to dispatch Elm messages up the tree.
elmMsgEvent :: EventType
elmMsgEvent = wrap "elm_msg_event"


-- | Attaches the initial info to a newly produced handler. The info is
-- | mutable, so this is an `Eff`
makeEventHandler :: ∀ e msg. HandlerInfo msg -> Eff (dom :: DOM, err :: EXCEPTION | e) (MsgHandler e msg)
makeEventHandler = eventHandler handleEvent


-- | The generic handler for any Elm event. We're doing something different
-- | here than Elm does. Instead of interally tracking "event nodes" and
-- | passing things up that chain, we apply our decoder and then re-dispatch a
-- | custom event with the results. So, whatever is above us in the DOM can do
-- | what it likes with that -- less book-keeping is needed.
handleEvent :: ∀ e msg. HandlerInfo msg -> Event -> Eff (dom :: DOM, err :: EXCEPTION | e) Unit
handleEvent info event =
    case decodeValue info.decoder (toForeign event) of
        Err _ ->
            -- Should we dispatch a distinct message that someone could listen
            -- to in order to catch the error? Might not be a bad idea.
            -- However, failing the decoder at this stage is a genuine strategy
            -- ... that way, you don't have to go through an `update` round.
            -- So, it's not as though this *necessarily* represents a bug.
            pure unit

        Ok msg -> do
            -- Perhaps curiously, the Elm code doesn't stopPropagation or
            -- preventDefault unless the decoder succeeds ... so, we'll do that
            -- as well.
            when info.options.preventDefault $
                preventDefault event

            when info.options.stopPropagation $
                stopPropagation event

            -- We re-dispatch the transformed message from the point in the
            -- DOM where the listener was attached, not necessarily where the
            -- event originated, since that will match the taggers up.
            msgEvent <-
                makeCustomEvent elmMsgEvent (toForeign msg)

            -- dispatchEvent adds the `EXCEPTION` effect
            void $ dispatchEvent (customEventToEvent msgEvent) (currentEventTarget event)


-- | Oddly, currentTarget returns a `Node` rather than an `EventTarget`, so
-- | we'll coerce. (Presumably the result of `currentTarget` must be an
-- | `EventTarget` seeing as, by definition, it has a listener attached).
currentEventTarget :: Event -> EventTarget
currentEventTarget = unsafeCoerce currentTarget


type TaggerHandler e a b =
    EventHandler (dom :: DOM, err :: EXCEPTION | e) (a -> b)


makeTagger :: ∀ e a b. (a -> b) -> Eff (dom :: DOM, err :: EXCEPTION | e) (TaggerHandler e a b)
makeTagger = eventHandler handleTagger


handleTagger :: ∀ e a b. (a -> b) -> Event -> Eff (dom :: DOM, err :: EXCEPTION | e) Unit
handleTagger tagger event =
    for_ (eventToCustomEvent event >>= detail) \msg -> do
        -- So, we're not giving ourselves very much type-safety here ... we're
        -- relying on the DOM to have been setup correctly so that our input is
        -- an `a` and we should be relaying a `b` upwards. There may be ways to
        -- improve on this.
        taggedMsg <-
            makeCustomEvent elmMsgEvent $ toForeign $ tagger $ unsafeCoerce msg

        maybeNext <-
            nextEventTarget $ currentTarget event

        for_ maybeNext \next -> do
            stopPropagation event
            dispatchEvent (customEventToEvent taggedMsg) next


--  DIFF

data PatchOp msg
    = PRedraw (Node msg)
    | PFacts (Array (FactChange msg))
    | PText String
    | PTagger (Exists (TaggerFunc msg))
    | PRemoveLast Int
    | PAppend (List (Node msg))
    | PCustom AnyRenderable AnyRenderable


newtype TaggerFunc msg a
    = TaggerFunc (a -> msg)


-- The index is a list of offsets to a root Node.
--
-- * If the patch relates to the root node itself, the list is empty.
--
-- * If the patch relates to a child of the root node, then the list
--   has one member, with the index into the children.
--
-- * And so on.
--
-- The idea is to make it easy to navigate the real DOM, once we get there, while
-- minimizing the iteration through the real DOM (which is postulated to be
-- expensive).
--
-- The original Elm code does this in a different way. There, the index is a
-- single `Int`, representing an offset into the nodes and children in order of
-- traversal. However, the code for actually applying this in `addDomNodes` ...
-- that is, for actually doing the traversal ...  is complex, difficult to
-- understand, and (for that reason) possibly fragile.
--
-- Now, always traversing from a root node using the list would, presumably, be
-- inefficient (or, at least, unoptimized). So, the strategy is to construct our
-- `List (Patch msg)` in such a way that traversing from one to the next ought to
-- be efficient. Then, we just need a function which, given two `List int`,
-- determines how to traverse the DOM efficiently from the first to the second.
-- So, in one case, it might be `nextSibling`, whereas in another case it might be
-- `parent` and then something else etc. In fact, I suppose this function ought to
-- get the root node as another parameter, since if you're sufficiently deep and
-- your next stop is sufficiently shallow, then it might make sense to start over
-- from the root.
--
-- In any event, the idea is that this will be more conceptually clear than the
-- Elm implementation, while hopefully preserving some of the efficiency of the
-- Elm implementation.
data Patch msg
    = Patch (List Int) (PatchOp msg)


-- The Elm version also includes a domNode and eventNode ... we add the domNode
-- later, and we've changed the implementation so that we don't use an eventNode
makePatch :: ∀ msg. PatchOp msg -> List Int -> Exists Patch
makePatch type_ index =
    mkExists $ Patch index type_


data Traversal
    = TRoot                 -- We're already there
    | TParent Int           -- go to the nth ancestor
    | TChild (List Int)     -- go to the child with the specified index, and repeat
    | TSibling SiblingRec


type SiblingRec =
    { up :: Int
    , from :: Int
    , to :: Int
    , down :: List Int
    }


instance showTraversal :: Show Traversal where
    show TRoot = "TRoot"
    show (TParent x) = "(TParent " <> show x <> ")"
    show (TChild x) = "(TChild " <> show x <> ")"
    show (TSibling s) = "(TSibling {up: " <> show s.up <> ", from: " <> show s.from <> ", to: " <> show s.to <> ", down: " <> show s.down <> "})"


-- We use this to calculate the most efficient traversal method ... that is, the one which
-- requires the fewest function calls.
--
-- My theory is that a child traversal costs 2, since you'll need to get the list of child nodes
-- and then index into them. But, I haven't really tested ... in theory, one could get actual
-- data for this.
costOfTraversal :: Traversal -> Int
costOfTraversal =
    case _ of
        TRoot ->
            0

        TParent x ->
            x

        TChild x ->
            length x * 2

        TSibling s ->
            s.up +
            (max 3 (abs (s.from - s.to))) +
            ((length s.down) * 2)


-- The first param is a sibling offset ... i.e. +1 for next sibling, -1 for
-- previous sibling, +2 for two siblings ahead, etc.
goSibling :: ∀ e. Int -> DOM.Node -> MaybeT (Eff (dom :: DOM | e)) DOM.Node
goSibling =
    tailRecM2 \which domNode ->
        if which == 0
            then
                pure $ Done domNode

            else
                if which > 0
                    then
                        nextSibling domNode
                        # MaybeT
                        <#> {a: which - 1, b: _}
                        <#> Loop

                    else
                        previousSibling domNode
                        # MaybeT
                        <#> {a: which + 1, b: _}
                        <#> Loop


-- Actually do the traversal ..
performTraversal :: ∀ e. Traversal -> DOM.Node -> MaybeT (Eff (dom :: DOM | e)) DOM.Node
performTraversal =
    tailRecM2 \t domNode ->
        case t of
            TRoot ->
                pure $ Done domNode

            TParent x ->
                if x > 0
                    then
                        parentNode domNode
                        # MaybeT
                        <#> {a: TParent (x - 1), b: _}
                        <#> Loop

                    else
                        pure $ Done domNode

            TChild (Cons c cs) ->
                childNodes domNode
                >>= item c
                # MaybeT
                <#> {a: TChild cs, b: _}
                <#> Loop

            TChild Nil ->
                pure $ Done domNode

            TSibling s ->
                let
                    distance =
                        s.to - s.from

                    sideways =
                        if abs distance > 3
                            then
                                performTraversal (TParent 1) >=>
                                performTraversal (TChild (singleton s.to))

                            else
                                goSibling distance

                in
                    performTraversal (TParent s.up) domNode
                    >>= sideways
                    <#> {a: TChild s.down, b: _}
                    <#> Loop


-- So, figure out how to move from current to destination.
traversal :: List Int -> List Int -> Traversal
traversal Nil Nil = TRoot                   -- it's all been equal, and nothing's left, so we're there
traversal Nil rest = TChild rest            -- Equal until something left on dest, so move to children
traversal rest Nil = TParent (length rest)  -- Equal until something left on source, so move to parents
traversal (Cons c cs) (Cons d ds) =         -- Something left on both sides, so take a look ...
    if c == d
        then
            -- If we're still equal, then just keep going
            traversal cs ds

        else
            -- Otherwise, we'll go up what's left on the source,
            -- from one sibling to the next, and then down what's
            -- left on the dest.
            TSibling
                { up: length cs
                , from: c
                , to: d
                , down: ds
                }


-- This represents a patch with a domNode filled in ... that is,
-- a patch where we've now determined exactly what it is going to patch.
data PatchWithNodes msg
    = PatchWithNodes (Patch msg) DOM.Node


diff :: ∀ msg. Node msg -> Node msg -> List (Exists Patch)
diff a b = diffHelp (Just id) a b Nil Nil


-- We accumulate the needed patches, by calling ourself recursively.  The `List
-- Int` tracks where we are relative to the root node.  An empty list meeans
-- we're at the root node, and then each list item represent an offset into
-- descendants.
--
-- Elm's Virtual DOM continues to diff children past taggers which are not
-- necessarily equal. Thus, we can't insist that the old node and the new node
-- have the same `msg` type -- we need to try to continue to diff even where
-- they do not. However, we use Leibniz equality to track whether we have
-- evidence that they are the same, so our behaviour can be a bit different in
-- one case vs. the other.
--
-- We use an existential type for accumulating patches in a single list.
--
-- TODO: Should probably not use a `List` for the offsets or the patches, since
-- we're appending.  Or, I should prepend and then document that you should
-- reverse once at the end.
diffHelp :: ∀ msg1 msg2. Maybe (msg1 ~ msg2) -> Node msg1 -> Node msg2 -> List Int -> List (Exists Patch) -> List (Exists Patch)
diffHelp proof a b index accum =
    -- We could have a distinct `equalNodes` function but it doesn't seem
    -- necessary ... we just return the patches unchanged if we're equal.
    if reallyUnsafeRefEq a b
        then accum
        else
            case a, b of
                Thunk aLazy aThunk, Thunk bLazy bThunk ->
                    if equalThunks aThunk bThunk then
                        -- The Elm code has an extra optimization here ... it
                        -- mutates the bLazy in-place so that it has the same
                        -- answer as the aLazy. That way, when we get it next
                        -- time, and might need to force it to do a diff, we
                        -- already have the answer. It's a sensible
                        -- optimization, but a little difficult to do safely in
                        -- Purescript. It can probably be done unsafely,
                        -- though, so it would be a nice TODO. It would
                        -- probably have to depend a little unsafely on the
                        -- internals of the `Lazy` type, or I suppose we could
                        -- build our own variant. It would be a kind of
                        -- `forceWith` where you supply the value that will be
                        -- forced, rather than running the calculation.
                        accum
                    else
                        -- The Elm code kind of restarts the diffing process
                        -- with the thunked node as the root, and then groups
                        -- the resulting patches together in a `PThunk` case.
                        -- I'm not sure whether that has a point ... it doesn't
                        -- seem to ... so I'll just calculate the actual nodes
                        -- and continue diffing in the usual manner ... it feels
                        -- as though that ought to just work. I guess we'll see!
                        --
                        -- The Elm code has pre-forced the `aLazy` side with the
                        -- optimization mentioned above ... would be nice to do
                        -- that here as well, but we'll start this way ... we'll
                        -- only pay when the thunks aren't equal.
                        diffHelp proof (force aLazy) (force bLazy) index accum

                Thunk2 aLazy aThunk, Thunk2 bLazy bThunk ->
                    -- See comments on the `Thunk` case
                    if equalThunks2 aThunk bThunk then
                        accum
                    else
                        diffHelp proof (force aLazy) (force bLazy) index accum

                Thunk3 aLazy aThunk, Thunk3 bLazy bThunk ->
                    -- See comments on the `Thunk` case
                    if equalThunks3 aThunk bThunk then
                        accum
                    else
                        diffHelp proof (force aLazy) (force bLazy) index accum

                Tagger aTagger, Tagger bTagger ->
                    -- Elm will diff the children whether or not the taggers
                    -- are equal, so long as there are the same number of
                    -- nested taggers.  So, it diffs children even we can't
                    -- know that they use the same `msg` type. The reason,
                    -- essentially, is that we can't always know that two
                    -- taggers are equal, even if they really are, so we want
                    -- to minimize the cost of those false negatives.  It
                    -- wouldn't be great to always have to redraw them, for
                    -- instance.
                    --
                    -- We check equality on the non-composed version, since
                    -- composition destroys the reference equality.
                    -- (equalTaggers will follow nested taggers).
                    case equalTaggers aTagger bTagger of
                        Just true ->
                            -- If the taggers were equal, then we don't need a
                            -- patch for the taggers, and we have warrant to
                            -- believe that the subnodes are of the same type.
                            --
                            -- We continue diffing at the same index, since the
                            -- tagger doesn't get a separate DOM node.
                            composeTaggers aTagger # unCoyoneda \_ node1 ->
                            composeTaggers bTagger # unCoyoneda \_ node2 ->
                                diffHelp (Just $ Leibniz unsafeCoerce) node1 node2 index accum

                        Just false ->
                            -- If the taggers are definitely unequal, then
                            -- we just redraw.
                            snoc accum (makePatch (PRedraw b) index)

                        Nothing ->
                            -- If we're not sure, then we need a patch for
                            -- the tagger itself, and also continue on.
                            -- But, without proof that the subnodes are the
                            -- same type.
                            composeTaggers aTagger # unCoyoneda \_ node1 ->
                            composeTaggers bTagger # unCoyoneda \func node2 ->
                                diffHelp Nothing node1 node2 index $
                                    snoc accum (makePatch (PTagger $ mkExists $ TaggerFunc func) index)

                Text aText, Text bText ->
                    if aText == bText
                        then accum
                        else snoc accum (makePatch (PText bText) index)

                PlainNode aNode aChildren, PlainNode bNode bChildren ->
                    if aNode.tag /= bNode.tag || aNode.namespace /= bNode.namespace
                        then snoc accum (makePatch (PRedraw b) index)
                        else
                            let
                                factsDiff =
                                    diffFacts proof aNode.facts bNode.facts

                                patchesWithFacts =
                                    if Array.null factsDiff
                                        then accum
                                        else snoc accum (makePatch (PFacts factsDiff) index)

                            in
                                -- diffChildren calls diffHelp, and I'm guessing that it
                                -- possibly won't be tail-recursive, because it's not calling
                                -- itself? So, may need to do something about that? Or
                                -- possibly it's not a problem.
                                diffChildren proof aChildren bChildren patchesWithFacts index

                KeyedNode aNode aChildren, KeyedNode bNode bChildren ->
                    unsafeCrashWith "TODO"

{-
		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;
-}

                Custom oldRenderable, Custom newRenderable ->
                    snoc accum (makePatch (PCustom oldRenderable newRenderable) index)
{-
		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
-}

                _, _ ->
                    -- This covers the case where they are different types
                    snoc accum (makePatch (PRedraw b) index)


-- Could optimize this stage out by writing a function that went straight
-- from OrganizedFacts to effects ... should try profiling at some point.
-- Also, there is probably a more efficient way to do this without all
-- the singleton list creation.
initialFactChanges :: ∀ msg. OrganizedFacts msg -> List (FactChange msg)
initialFactChanges facts =
    attributeChanges <>
    attributeNsChanges <>
    eventChanges <>
    styleChanges <>
    propertyChanges

    where
        attributeChanges =
            facts.attributes #
                foldMap \k v ->
                    singleton (AddAttribute k v)

        attributeNsChanges =
            facts.attributesNS #
                foldMap \ns ->
                    foldMap \k v ->
                        singleton (AddAttributeNS ns k v)

        eventChanges =
            facts.events #
                foldMap \k (Tuple options decoder) ->
                    singleton (AddEvent (wrap k) options decoder)

        styleChanges =
            facts.styles #
                foldMap \k v ->
                    singleton (AddStyle k v)

        propertyChanges =
            facts.properties #
                foldMap \k v ->
                    singleton (AddProperty k v)


diffFacts :: ∀ oldMsg newMsg. Maybe (oldMsg ~ newMsg) -> OrganizedFacts oldMsg -> OrganizedFacts newMsg -> Array (FactChange newMsg)
diffFacts proof old new =
    runPure do
        runSTArray do
            -- I suppose the other alternative would be a Writer monad ... perhaps
            -- that would be better?
            accum <- emptySTArray

            let
                newProperty _ k newValue =
                    case lookup k old.properties of
                        Just oldValue ->
                            -- Where the key is "value", we defer the check to
                            -- the actual DOM, rather than the virtual DOM, because
                            -- the browser will change "value" behind our backs.
                            -- Possibly "checked" should be handled this way as well:
                            -- https://github.com/elm-lang/virtual-dom/issues/117
                            --
                            -- The value is a Foreign, so the best we can do is
                            -- an unsafeRefEq here, I suppose?
                            when ((not (unsafeRefEq newValue oldValue)) || (k == "value")) $ void $
                                pushSTArray accum (AddProperty k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddProperty k newValue)

                oldProperty _ k oldValue =
                    case lookup k new.properties of
                        Just _ ->
                            -- We'll have done this one already ...
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveProperty k)

                newAttribute _ k newValue =
                    case lookup k old.attributes of
                        Just oldValue ->
                            unless (newValue == oldValue) $ void $
                                pushSTArray accum (AddAttribute k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddAttribute k newValue)

                oldAttribute _ k oldValue =
                    case lookup k new.attributes of
                        Just _ ->
                            -- We'll have done this one already ...
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveAttribute k)

                newAttributeNS _ ns newSubmap =
                    let
                        submapper _ k newValue =
                            case lookup ns old.attributesNS of
                                Just oldSubmap ->
                                    -- We had something in this namespace, so iterate
                                    case lookup k oldSubmap of
                                        Just oldValue ->
                                            unless (newValue == oldValue) $ void $
                                                pushSTArray accum (AddAttributeNS ns k newValue)

                                        Nothing ->
                                            void $
                                                pushSTArray accum (AddAttributeNS ns k newValue)

                                Nothing ->
                                    -- There wasn't anything in this namespace, so
                                    -- we'll need to add them all.
                                    void $
                                        pushSTArray accum (AddAttributeNS ns k newValue)

                    in
                        foldM submapper unit newSubmap

                oldAttributeNS _ ns oldSubmap =
                    let
                        submapper _ k value =
                            case lookup ns new.attributesNS of
                                Just newSubmap ->
                                    case lookup k newSubmap of
                                        Just newValue ->
                                            -- We'll have checked the newValue already
                                            pure unit

                                        Nothing ->
                                            void $
                                                pushSTArray accum (RemoveAttributeNS ns k)

                                Nothing ->
                                    -- There now isn't anything in this namespace, so remove all
                                    void $
                                        pushSTArray accum (RemoveAttributeNS ns k)

                    in
                        foldM submapper unit oldSubmap

                newStyle _ k newValue =
                    case lookup k old.styles of
                        Just oldValue ->
                            unless (newValue == oldValue) $ void $
                                pushSTArray accum (AddStyle k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddStyle k newValue)

                oldStyle _ k oldValue =
                    case lookup k new.styles of
                        Just _ ->
                             -- We'll handle this when iterating the new stuff.
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveStyle k)

                newEvent _ k (newOptions /\ newDecoder) =
                    case lookup k old.events of
                        Just (oldOptions /\ oldDecoder ) ->
                            unless (equalOptions oldOptions newOptions && equalDecodersL proof oldDecoder newDecoder) do
                                void $ pushSTArray accum (MutateEvent (wrap k) newOptions newDecoder)

                        Nothing ->
                            void $
                                pushSTArray accum (AddEvent (wrap k) newOptions newDecoder)

                oldEvent _ k (oldOptions /\ oldDecoder) =
                    case lookup k new.events of
                        Just _ ->
                            -- We'll handle this when iterating the new stuff.
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveEvent (wrap k) oldOptions)


            -- Push removals first, then additions. I've forgotten why each of
            -- these has an unused initial parameter ...
            foldM oldAttribute unit old.attributes
            foldM oldAttributeNS unit old.attributesNS
            foldM oldProperty unit old.properties
            foldM oldEvent unit old.events

            foldM newAttribute unit new.attributes
            foldM newAttributeNS unit new.attributesNS
            foldM newProperty unit new.properties
            foldM newEvent unit new.events

            case Tuple (isEmpty old.styles) (isEmpty new.styles) of
                Tuple false true ->
                    -- It wasn't empty but now is, so remove all
                    void $ pushSTArray accum RemoveAllStyles

                Tuple true true ->
                    -- It was empty and still is, so do nothing
                    pure unit

                Tuple false false -> do
                    -- There were some, and still are, so look
                    -- at both
                    foldM oldStyle unit old.styles
                    foldM newStyle unit new.styles

                Tuple true false ->
                    -- It was empty, and now isn't. So, just add.
                    foldM newStyle unit new.styles

            pure accum


diffChildren :: ∀ msg1 msg2. Maybe (msg1 ~ msg2) -> List (Node msg1) -> List (Node msg2) -> List (Exists Patch) -> List Int -> List (Exists Patch)
diffChildren proof aChildren bChildren patches rootIndex =
    let
        aLen = length aChildren
        bLen = length bChildren

        insertsAndRemovals =
            if aLen > bLen
                then snoc patches (makePatch (PRemoveLast (aLen - bLen)) rootIndex)
                else
                    if aLen < bLen
                        then snoc patches (makePatch (PAppend (drop aLen bChildren)) rootIndex)
                        else patches

        pairs =
            zip aChildren bChildren

        diffPairs =
            foldl diffChild { subIndex: 0, patches: insertsAndRemovals } pairs

        diffChild memo (Tuple aChild bChild) =
            { subIndex: memo.subIndex + 1
            , patches: diffHelp proof aChild bChild (snoc rootIndex memo.subIndex) memo.patches
            }

    in
        diffPairs.patches


{-
function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}
-}


addDomNodes :: ∀ e. DOM.Node -> List (Exists Patch) -> Eff (dom :: DOM | e) (List (Exists PatchWithNodes))
addDomNodes rootNode patches = do
    patches
        # List.foldM step
            { currentNode: rootNode
            , currentIndex: Nil
            , accum: Nil
            }
        <#> \result ->
            reverse result.accum

    where
        step params = runExists \patch@(Patch index _) -> do
            let
                fromRoot =
                    traversal Nil index

                fromCurrent =
                    traversal params.currentIndex index

                best =
                    if (costOfTraversal fromRoot) < (costOfTraversal fromCurrent)
                        then performTraversal fromRoot rootNode
                        else performTraversal fromCurrent params.currentNode

            maybeDest <-
                runMaybeT best

            case maybeDest of
                Just domNode ->
                    let
                        patchWithNodes =
                            mkExists $ PatchWithNodes patch domNode
                    in
                        pure
                            { currentNode: domNode
                            , currentIndex: index
                            , accum: Cons patchWithNodes params.accum
                            }

                Nothing ->
                    -- Now, I think crashing if the DOM has been unexpectedly modified is probably
                    -- the right thing to do, since all bets are then off. I suppose it's either that,
                    -- or revert to a complete redraw? In any event, the other question is where to
                    -- handle this problem ... we could just throw an exception here and let someone
                    -- else handle it. Also, we should add some more information, to help with debugging.
                    unsafeCrashWith "Problem traversing DOM -- has it been modified from the outside?"


-- APPLY PATCHES

applyPatches :: ∀ e. DOM.Node -> List (Exists Patch) -> EffDOM e DOM.Node
applyPatches rootDomNode patches =
    if List.null patches
        then
            pure rootDomNode

        else
            addDomNodes rootDomNode patches
            >>= applyPatchesHelp rootDomNode


applyPatchesHelp :: ∀ e. DOM.Node -> List (Exists PatchWithNodes) -> EffDOM e DOM.Node
applyPatchesHelp =
    List.foldM \rootNode ->
        runExists \(PatchWithNodes patch@(Patch index _) domNode) -> do
            newNode <- applyPatch patch domNode

            -- This is a little hackish ... should think this through better.
            -- But, basically the idea is that if the patch index was nil,
            -- we want to go on with the newNode. If not ... that is, if
            -- we were patching further on ... then we don't want to change
            -- the rootNode.
            if index == Nil
                then pure newNode
                else pure rootNode


applyPatch :: ∀ e msg. Patch msg -> DOM.Node -> EffDOM e DOM.Node
applyPatch (Patch index patchOp) domNode = do
    document <-
        documentForNode domNode

    case patchOp of
        PRedraw vNode ->
            redraw domNode vNode

        PFacts changes -> do
            nodeToElement domNode
                <#> applyFacts changes
                # fromMaybe (pure unit)

            pure domNode

        PText string -> do
            setTextContent string domNode
            pure domNode

        PTagger taggerFunc -> do
            -- See comment in `render` ... I believe we can only apply a tagger
            -- to an element, but it might be good to verify that.
            for_ (nodeToElement domNode) \element ->
                taggerFunc # runExists \(TaggerFunc func) -> do
                    tagger <-
                        toMaybe <$> getTagger element

                    case tagger of
                        Just t ->
                            setHandlerInfo func t

                        Nothing ->
                            unsafeCrashWith "Could not find expected tagger in DOM"

            pure domNode

        PRemoveLast howMany -> do
            -- There must be a replicateM somewhere I'm forgetting ...
            forE 0 howMany \_ ->
                lastChild domNode >>=
                    case _ of
                        Just child -> do
                            void $ removeChild child domNode

                        Nothing ->
                            pure unit

            pure domNode

        PAppend newNodes -> do
            for_ newNodes $
                render document >=> (flip appendChild) domNode

            pure domNode

{-
		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			var data = patch.data;

            // remove end inserts
            var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

            // removals
            domNode = applyPatchesHelp(domNode, data.patches);

            // inserts
            var inserts = data.inserts;
            for (var i = 0; i < inserts.length; i++)
            {
                var insert = inserts[i];
                var entry = insert.entry;
                var node = entry.tag === 'move'
                    ? entry.data
                    : render(entry.vnode, patch.eventNode);
                domNode.insertBefore(node, domNode.childNodes[insert.index]);
            }

            // add end inserts
            if (typeof frag !== 'undefined')
            {
                domNode.appendChild(frag);
            }

            return domNode;

function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}
-}

        PCustom old new ->
            Renderable.updateDOM
                { value: old
                , result: domNode
                , document
                }
                new
            <#> \x -> x.result

            {-
            var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);
            -}


redraw :: ∀ e msg. DOM.Node -> Node msg -> EffDOM e DOM.Node
redraw domNode vNode = do
    document <- documentForNode domNode
    parentNode <- parentNode domNode
    newNode <- render document vNode

    -- We transfer a tagger from the old node to the new node, if there was
    -- one, because taggers don't get their own node ... so we neeed to
    -- preserve them when redrawing.
    for_ (nodeToElement domNode) \oldElement ->
        for_ (nodeToElement newNode) \newElement -> do
            tagger <-
                toMaybe <$> getTagger oldElement

            for_ tagger (flip setTagger newElement)

    case parentNode of
        Just p -> do
            void $ replaceChild newNode domNode p
            pure newNode

        Nothing ->
            pure newNode


-- | > Check out the docs for [`Html.App.program`][prog].
-- | > It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#program
program :: ∀ flags model msg.
    { init :: Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    , view :: model -> Node msg
    }
    -> Program flags model msg
program config =
    programWithFlags $
        config { init = const config.init }


-- | > Check out the docs for [`Html.App.programWithFlags`][prog].
-- | > It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#programWithFlags
programWithFlags :: ∀ flags model msg.
    { init :: flags -> Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    , view :: model -> Node msg
    }
    -> Program flags model msg
programWithFlags config =
    wrap
        { init : config.init
        , update : config.update
        , subscriptions : config.subscriptions
        , view : Just (renderer config.view)
        }


renderer :: ∀ model msg. (model -> Node msg) -> AVar msg -> IO (AVar model)
renderer view mailbox = liftAff do
    models <-
        makeEmptyVar

    -- Eventually this will need to be parameterized to make `embed`
    -- work ... for the moment, just assume `fullScreen`
    doc <-
        liftEff $ window >>= document

    -- TODO: Integrate with requestAnimationFrame
    let loop state = do
            newModel <-
                takeVar models

            let newView = view newModel

            newNode <- liftEff
                case state of
                    Nothing -> do
                        newNode <-
                            render (htmlDocumentToDocument doc) (view newModel)

                        maybeBody <-
                            body doc

                        let listener =
                                eventListener \event ->
                                    for_ (eventToCustomEvent event >>= detail) \msg ->
                                        -- This is a bit hackish ... we rely on things having
                                        -- been set up correctly so that the msg is of the
                                        -- correct type ... should revisit at some point.
                                        --
                                        -- Also, we're (necessarily?) in an Eff context here.
                                        -- So, we supply a callback that does nothing, and
                                        -- we get a canceler back which we're ignoring. Should
                                        -- think about what that means in terms of control flow.
                                        putVar (unsafeCoerce msg) mailbox (const $ pure unit)

                        for_ maybeBody \b -> do
                            void $ appendChild newNode (htmlElementToNode b)
                            addEventListener elmMsgEvent listener false (htmlElementToEventTarget b)

                        pure newNode

                    Just (oldView /\ oldNode) -> do
                        void $ applyPatches oldNode $
                            diff oldView newView

                        pure oldNode

            loop $ Just (newView /\ newNode)

    void $ forkAff $ loop Nothing

    pure models


{-

// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;
				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}

-}
