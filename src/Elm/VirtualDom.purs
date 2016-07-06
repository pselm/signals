
-- | API to the core diffing algorithm. Can serve as a foundation for libraries
-- | that expose more helper functions for HTML or SVG.

module Elm.VirtualDom
    ( Node, text, node
    , Property, property, attribute, attributeNS, style
    , Options, on, onWithOptions, defaultOptions
    , map
    , lazy, lazy2, lazy3
--  , custom
--  , programWithFlags
    ) where


import Elm.Json.Decode (Decoder, Value) as Json
import Elm.Basics (Bool)
import Elm.Graphics.Internal
    ( setStyle, removeStyle
    , setProperty, setPropertyIfDifferent, removeProperty
    , setAttributeNS, removeAttributeNS, nodeToElement
    , documentForNode
    )

import Control.Monad.ST (pureST, newSTRef, writeSTRef, readSTRef)
import Control.Monad.Eff (Eff, runPure, forE)
import Control.Monad.Rec.Class (tailRecM2)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad (unless, (>=>))
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafeCrashWith)

import Data.Array (null) as Array
import Data.Tuple (Tuple(..))
import Data.List (List(..), length, reverse, singleton, snoc, drop, zip, (:))
import Data.List (foldM, singleton, null) as List
import Data.Array.ST (runSTArray, emptySTArray, pushSTArray)
import Data.Foldable (class Foldable, foldl, for_)
import Data.StrMap (StrMap, foldM, foldMap, lookup)
import Data.StrMap.ST (new, poke, peek)
import Data.StrMap.ST.Unsafe (unsafeGet)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Exists (Exists, mkExists)
import Data.Foreign (readString)
import Data.Ord (abs, max)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Nullable (toNullable, toMaybe)

import DOM (DOM)
import DOM.Node.NodeList (item)
import DOM.Node.Node (appendChild, parentNode, replaceChild, lastChild, setTextContent, removeChild, childNodes, nextSibling, previousSibling)
import DOM.Node.Types (Document, Element, textToNode, elementToNode)
import DOM.Node.Types (Node) as DOM
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (setAttribute, removeAttribute)
import DOM.Renderable (class Renderable)

import Prelude (class Eq, class Show, show, Unit, unit, flip, (+), (-), (*), void, pure, bind, (>>=), ($), (<$>), (<#>), (==), (/=), (||), (#), (<>), (<), (>))


-- Will suggest these for Data.Exists if they work
foreign import data Exists2 :: (* -> * -> *) -> *

mkExists2 :: ∀ f a b. f a b -> Exists2 f
mkExists2 = unsafeCoerce

runExists2 :: ∀ f r. (∀ a b. f a b -> r) -> Exists2 f -> r
runExists2 = unsafeCoerce


foreign import data Exists3 :: (* -> * -> * -> *) -> *

mkExists3 :: ∀ f a b c. f a b c -> Exists3 f
mkExists3 = unsafeCoerce

runExists3 :: ∀ f r. (∀ a b c. f a b c -> r) -> Exists3 f -> r
runExists3 = unsafeCoerce


-- The original Javascript code uses reference equality on occasion ...
foreign import refEq :: ∀ a b. a -> b -> Boolean


-- | An immutable chunk of data representing a DOM node. This can be HTML or SVG.
data Node msg
    = Text String
    | PlainNode (NodeRecord msg)
    | Tagger (Exists (TaggerRecord msg))
    | Thunk (Exists (ThunkRecord1 msg))
    | Thunk2 (Exists2 (ThunkRecord2 msg))
    | Thunk3 (Exists3 (ThunkRecord3 msg))
    | Custom


type NodeRecord msg =
    { tag :: String
    , namespace :: Maybe String
    , facts :: OrganizedFacts msg
    , children :: List (Node msg)
    }


newtype TaggerRecord msg sub = TaggerRecord
    { tagger :: sub -> msg
    , child :: Node sub
    }


instance renderableNode :: Renderable (Node msg) where
    render document n =
        render document n $
            EventNode
                { tagger: 0
                , parent: Nothing
                }

    update old current =
        applyPatches old.result old.value (diff old.value current) $
            EventNode
                { tagger: 0
                , parent: Nothing
                }


-- This should really be generalized and broken out. It has something
-- in common with laziness, and something in common with memoization,
-- but isn't quite the same as either. I suppose it is fundamentally
-- a lazy calculation which is able to decide whether it is equal
-- to another lazy calculation. So, if you have already forced one,
-- you don't need to force another one that is equal.
--
-- Like the Elm version of this, we're relying on reference equality.
-- Doing anything else would be a bit puzzling, unless we complicated
-- the type of `Node` a great deal. The existential types help to a point,
-- but prevent us (of course) from knowing that two things have the same
-- type. I suppose this may be another case where typeable might help?
-- There would also be the question of function equality to deal with.
newtype ThunkRecord1 msg a = ThunkRecord1
    { func :: a -> Node msg
    , arg :: a
    , lazy :: Lazy (Node msg)
    }


newtype ThunkRecord2 msg a b = ThunkRecord2
    { func :: a -> b -> Node msg
    , arg1 :: a
    , arg2 :: b
    , lazy :: Lazy (Node msg)
    }


newtype ThunkRecord3 msg a b c = ThunkRecord3
    { func :: a -> b -> c -> Node msg
    , arg1 :: a
    , arg2 :: b
    , arg3 :: c
    , lazy :: Lazy (Node msg)
    }


-- | Create a DOM node with a tag name, a list of HTML properties that can
-- | include styles and event listeners, a list of CSS properties like `color`, and
-- | a list of child nodes.
-- |
-- |     import Json.Encode as Json
-- |
-- |     hello :: Node msg
-- |     hello =
-- |       node "div" [] [ text "Hello!" ]
-- |
-- |     greeting :: Node msg
-- |     greeting =
-- |       node "div"
-- |         [ property "id" (Json.string "greeting") ]
-- |         [ text "Hello!" ]
node :: ∀ msg. String -> List (Property msg) -> List (Node msg) -> Node msg
node tag properties children =
    PlainNode $
        { tag
        , namespace: organized.namespace
        , children
        , facts: organized.facts
        }

    where
        organized =
            organizeFacts properties


-- | Works just like `node`, but you add a unique identifier to each child
-- | node. You want this when you have a list of nodes that is changing: adding
-- | nodes, removing nodes, etc. In these cases, the unique identifiers help make
-- | the DOM modifications more efficient.
-- keyedNode :: String -> List (Property msg) -> List (Tuple String (Node msg)) -> Node msg


{-
function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}
-}


-- | Just put plain text in the DOM. It will escape the string so that it appears
-- | exactly as you specify.
-- |
-- |     text "Hello World!"
text :: ∀ msg. String -> Node msg
text = Text


-- | This function is useful when nesting components with [the Elm
-- | Architecture](https://github.com/evancz/elm-architecture-tutorial/). It lets
-- | you transform the messages produced by a subtree.
-- |
-- | Say you have a node named `button` that produces `()` values when it is
-- | clicked. To get your model updating properly, you will probably want to tag
-- | this `()` value like this:
-- |
-- |     type Msg = Click | ...
-- |
-- |     update msg model =
-- |       case msg of
-- |         Click ->
-- |           ...
-- |
-- |     view model =
-- |       map (\_ -> Click) button
-- |
-- | So now all the events produced by `button` will be transformed to be of type
-- | `Msg` so they can be handled by your update function!
map :: ∀ sub msg. (sub -> msg) -> Node sub -> Node msg
map tagger child =
    Tagger (mkExists (TaggerRecord {tagger, child}))


-- PROPERTIES

-- | When using HTML and JS, there are two ways to specify parts of a DOM node.
-- |
-- |   1. Attributes &mdash; You can set things in HTML itself. So the `class`
-- |      in `<div class="greeting"></div>` is called an *attribute*.
-- |
-- |   2. Properties &mdash; You can also set things in JS. So the `className`
-- |      in `div.className = 'greeting'` is called a *property*.
-- |
-- | So the `class` attribute corresponds to the `className` property. At first
-- | glance, perhaps this distinction is defensible, but it gets much crazier.
-- | *There is not always a one-to-one mapping between attributes and properties!*
-- | Yes, that is a true fact. Sometimes an attribute exists, but there is no
-- | corresponding property. Sometimes changing an attribute does not change the
-- | underlying property. For example, as of this writing, the `webkit-playsinline`
-- | attribute can be used in HTML, but there is no corresponding property!
data Property msg
    = CustomProperty Key Json.Value
    | Attribute Key String
    | AttributeNS Namespace Key String
    | Styles (List (Tuple String String))
    | OnEvent Key Options (Json.Decoder msg)


-- This represents the properties that should be applied to a node, organized for
-- quick lookup (since the API supplies them as a list).
type OrganizedFacts msg =
    { attributes :: StrMap String
    , attributesNS :: StrMap (StrMap String)
    , events :: StrMap (Tuple Options (Json.Decoder msg))
    , styles :: StrMap String
    , properties :: StrMap Json.Value
    }


data FactChange msg
    = AddAttribute String String
    | RemoveAttribute String
    | AddAttributeNS String String String
    | RemoveAttributeNS String String
    | AddEvent String Options (Json.Decoder msg)
    | RemoveEvent String Options
    | AddStyle String String
    | RemoveStyle String
    | AddProperty String Json.Value
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

                    poke submap key value
                    poke mutableAttributesNS ns submap

                OnEvent key options decoder ->
                    void $ poke mutableEvents key (Tuple options decoder)

                Styles list ->
                    for_ list \(Tuple key value) ->
                        poke mutableStyles key value

                CustomProperty key value ->
                    -- So, the normal case here is that we're setting an arbitrary property
                    -- on the node. However, the original Elm code also allows you to use
                    -- a special "namespace" property, to specify the namespace of the node
                    -- itself. This seems a bit odd ... would probably be better to have an
                    -- explicit API for namespaced nodes.
                    if key == "namespace"
                        then
                            case readString value of
                                Left _ ->
                                    -- It wasn't a string, so don't handle specially
                                    void $ poke mutableProperties key value

                                Right s ->
                                    -- It was a string, so store as the namespace
                                    void $ writeSTRef mutableNamespace (Just s)

                        else
                            void $ poke mutableProperties key value

        -- These are unsafe in the sense that further modifications to the mutable
        -- versions would also modify the pure. So, we won't do that ...
        -- The alternative is freezeST, but that actually does a copy, which in
        -- this context isn't really necessary.
        attributes <- unsafeGet mutableAttributes
        events <- unsafeGet mutableEvents
        styles <- unsafeGet mutableStyles
        properties <- unsafeGet mutableProperties

        -- I also need to iterate over all of the submaps and "freeze" them ...
        -- and then freeze the resulting outer map
        pureOuterMap <- unsafeGet mutableAttributesNS
        accumulator <- new

        foldM (\accum key submap ->
            unsafeGet submap >>= poke accum key
        ) accumulator pureOuterMap

        attributesNS <- unsafeGet accumulator

        -- And read the namespace
        namespace <- readSTRef mutableNamespace

        pure
            { namespace
            , facts: { attributes, attributesNS, events, styles, properties }
            }



-- | Create arbitrary *properties*.
-- |
-- |     import JavaScript.Encode as Json
-- |
-- |     greeting : Html
-- |     greeting =
-- |         node "div" [ property "className" (Json.string "greeting") ] [
-- |           text "Hello!"
-- |         ]
-- |
-- | Notice that you must give the *property* name, so we use `className` as it
-- | would be in JavaScript, not `class` as it would appear in HTML.
property :: ∀ msg. String -> Json.Value -> Property msg
property = CustomProperty


-- | Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
-- | function under the hood.
-- |
-- |     greeting : Html
-- |     greeting =
-- |         node "div" [ attribute "class" "greeting" ] [
-- |           text "Hello!"
-- |         ]
-- |
-- | Notice that you must give the *attribute* name, so we use `class` as it would
-- | be in HTML, not `className` as it would appear in JS.
attribute :: ∀ msg. String -> String -> Property msg
attribute = Attribute


-- | Would you believe that there is another way to do this?! This corresponds
-- | to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
-- | much the same thing as `attribute` but you are able to have "namespaced"
-- | attributes. This is used in some SVG stuff at least.
attributeNS :: ∀ msg. String -> String -> String -> Property msg
attributeNS = AttributeNS


-- | Specify a list of styles.
-- |
-- |     myStyle :: Property msg
-- |     myStyle =
-- |       style
-- |         [ Tuple "backgroundColor" "red"
-- |         , Tuple "height" "90px"
-- |         , Tuple "width" "100%"
-- |         ]
-- |
-- |     greeting :: Node msg
-- |     greeting =
-- |       node "div" [ myStyle ] [ text "Hello!" ]
style :: ∀ msg. List (Tuple String String) -> Property msg
style = Styles


-- EVENTS

-- | Create a custom event listener.
-- |
-- |     import Json.Decode as Json
-- |
-- |     onClick : msg -> Property msg
-- |     onClick msg =
-- |       on "click" (Json.succeed msg)
-- |
-- | You first specify the name of the event in the same format as with JavaScript’s
-- | `addEventListener`. Next you give a JSON decoder, which lets you pull
-- | information out of the event object. If the decoder succeeds, it will produce
-- | a message and route it to your `update` function.
on :: ∀ msg. String -> Json.Decoder msg -> Property msg
on =
    (flip onWithOptions) defaultOptions


-- | Same as `on` but you can set a few options.
onWithOptions :: ∀ msg. String -> Options -> Json.Decoder msg -> Property msg
onWithOptions = OnEvent


-- | Options for an event listener. If `stopPropagation` is true, it means the
-- | event stops traveling through the DOM so it will not trigger any other event
-- | listeners. If `preventDefault` is true, any built-in browser behavior related
-- | to the event is prevented. For example, this is used with touch events when you
-- | want to treat them as gestures of your own, not as scrolls.
type Options =
    { stopPropagation :: Bool
    , preventDefault :: Bool
    }


-- | Everything is `False` by default.
-- |
-- |     defaultOptions =
-- |         { stopPropagation = False
-- |         , preventDefault = False
-- |         }
defaultOptions :: Options
defaultOptions =
    { stopPropagation: false
    , preventDefault: false
    }


-- OPTIMIZATION

-- | A performance optimization that delays the building of virtual DOM nodes.
-- |
-- | Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
-- | it. Calling `(lazy view model)` delays the call until later. During diffing, we
-- | can check to see if `model` is referentially equal to the previous value used,
-- | and if so, we just stop. No need to build up the tree structure and diff it,
-- | we know if the input to `view` is the same, the output must be the same!
lazy :: ∀ a msg. (Eq a) => (a -> Node msg) -> a -> Node msg
lazy func arg =
    Thunk (mkExists (ThunkRecord1 {func, arg, lazy: defer \_ -> func arg}))


-- | Same as `lazy` but checks on two arguments.
lazy2 :: ∀ a b msg. (Eq a, Eq b) => (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 func arg1 arg2 =
    Thunk2 (mkExists2 (ThunkRecord2 {func, arg1, arg2, lazy: defer \_ -> func arg1 arg2}))


-- | Same as `lazy` but checks on three arguments.
lazy3 :: ∀ a b c msg. (Eq a, Eq b, Eq c) => (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 func arg1 arg2 arg3 =
    Thunk3 (mkExists3 (ThunkRecord3 {func, arg1, arg2, arg3, lazy: defer \_ -> func arg1 arg2 arg3}))


{-

-- This one is going to be a bit tricky, because I've implemented Json.Decode in a
-- way that isn't going to be great for testing equality for Json.Decoder. I might
-- have to switch Json.Decode to more of a free monad approach. Which, in fact, would
-- roughly correspond to some changes in Json.Decode for Elm 0.17.

function equalEvents(a, b)
{
	if (!a.options === b.options)
	{
		if (a.stopPropagation !== b.stopPropagation || a.preventDefault !== b.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}

-}


-- RENDERER

newtype EventNode = EventNode
    { tagger :: Int
    , parent :: Maybe EventNode
    }


{-
function renderer(parent, tagger, initialVirtualNode)
{
	var eventNode = { tagger: tagger, parent: null };

	var domNode = render(initialVirtualNode, eventNode);
	parent.appendChild(domNode);

	var state = 'NO_REQUEST';
	var currentVirtualNode = initialVirtualNode;
	var nextVirtualNode = initialVirtualNode;

	function registerVirtualNode(vNode)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextVirtualNode = vNode;
	}

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/core/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var patches = diff(currentVirtualNode, nextVirtualNode);
				domNode = applyPatches(domNode, currentVirtualNode, patches, eventNode);
				currentVirtualNode = nextVirtualNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return { update: registerVirtualNode };
}


var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(cb) { setTimeout(cb, 1000 / 60); };

-}


-- RENDER

render :: ∀ e msg. Document -> Node msg -> EventNode -> Eff (dom :: DOM | e) DOM.Node
render doc vNode eventNode = do
    case vNode of
        Thunk t ->
            unsafeCrashWith "TODO"

{-
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);
-}

        Thunk2 t ->
            unsafeCrashWith "TODO"

        Thunk3 t ->
            unsafeCrashWith "TODO"

        Text string ->
            createTextNode string doc
            <#> textToNode

        PlainNode rec -> do
            domNode <-
                case rec.namespace of
                    Just ns ->
                        createElementNS (toNullable rec.namespace) rec.tag doc

                    Nothing ->
                        createElement rec.tag doc

            applyFacts eventNode (initialFactChanges rec.facts) domNode

            for_ rec.children \child -> do
                renderedChild <- render doc child eventNode
                appendChild renderedChild (elementToNode domNode)

            pure (elementToNode domNode)

        Tagger rec ->
            unsafeCrashWith "TODO"

{-
            var subEventRoot = {
				tagger: vNode.tagger,
				parent: eventNode
			};
			var domNode = render(vNode.node, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;
-}

        Custom ->
            unsafeCrashWith "TODO"

{-
		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
-}


-- APPLY FACTS

applyFacts :: ∀ e f msg. (Foldable f) => EventNode -> f (FactChange msg) -> Element -> Eff (dom :: DOM | e) Unit
applyFacts eventNode operations elem = do
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

            AddEvent key options decoder ->
                unsafeCrashWith "TODO"

            RemoveEvent key options ->
                unsafeCrashWith "TODO"

            AddStyle key value ->
                setStyle key value elem

            RemoveStyle key ->
                removeStyle key elem

            AddProperty key value ->
                if key == "value"
                    -- I think this is trying to avoid setting the "value" property if it is not
                    -- changing, probably to avoid some kind of browser issue.
                    then setPropertyIfDifferent key value elem
                    else setProperty key value elem

            RemoveProperty key ->
                removeProperty key elem


{-
function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}
-}


--  DIFF

data PatchOp msg
    = PRedraw (Node msg)
    | PFacts (Array (FactChange msg))
    | PText String
    | PThunk
    | PTagger
    | PRemoveLast Int
    | PAppend (List (Node msg))
    | PCustom


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
type Patch msg =
    { index :: List Int
    , type_ :: PatchOp msg
    }


makePatch :: ∀ msg. PatchOp msg -> List Int -> Patch msg
makePatch type_ index =
    { index
    , type_
    }


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


-- My theory is that a child traversal costs 2, since you'll need to get the list of child nodes
-- and then index into them. But, I haven't really tested ... in theory, one could get actual
-- data for this.
costOfTraversal :: Traversal -> Int
costOfTraversal TRoot = 0
costOfTraversal (TParent x)  = x
costOfTraversal (TChild x) = (length x) * 2
costOfTraversal (TSibling s) = s.up + (max 3 (abs (s.from - s.to))) + ((length s.down) * 2)


-- The first param is a sibling offset ... i.e. +1 for next sibling, -1 for
-- previous sibling, +2 for two siblings ahead, etc.
goSibling :: ∀ e. Int -> DOM.Node -> MaybeT (Eff (dom :: DOM | e)) DOM.Node
goSibling =
    tailRecM2 \which domNode ->
        if which == 0
            then
                pure $ Right domNode

            else
                if which > 0
                    then
                        nextSibling domNode
                        <#> toMaybe # MaybeT
                        <#> {a: which - 1, b: _}
                        <#> Left

                    else
                        previousSibling domNode
                        <#> toMaybe # MaybeT
                        <#> {a: which + 1, b: _}
                        <#> Left


-- Actually do the traversal ..
performTraversal :: ∀ e. Traversal -> DOM.Node -> MaybeT (Eff (dom :: DOM | e)) DOM.Node
performTraversal =
    tailRecM2 \t domNode ->
        case t of
            TRoot ->
                pure $ Right domNode

            TParent x ->
                if x > 0
                    then
                        parentNode domNode
                        <#> toMaybe # MaybeT
                        <#> {a: TParent (x - 1), b: _}
                        <#> Left

                    else
                        pure $ Right domNode

            TChild (Cons c cs) ->
                childNodes domNode
                >>= item c
                <#> toMaybe # MaybeT
                <#> {a: TChild cs, b: _}
                <#> Left

            TChild Nil ->
                pure $ Right domNode

            TSibling s ->
                let
                    distance =
                        s.to - s.from

                    sideways =
                        if abs distance > 3
                            then
                                performTraversal (TParent 1) >=>
                                performTraversal (TChild (List.singleton s.to))

                            else
                                goSibling distance

                in
                    performTraversal (TParent s.up) domNode
                    >>= sideways
                    <#> {a: TChild s.down, b: _}
                    <#> Left


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


-- This represents a patch with a domNode and event node filled in ... that is,
-- a patch where we've now determined exactly what it is going to patch.
type PatchWithNodes msg =
    { patch :: Patch msg
    , domNode :: DOM.Node
    , eventNode :: EventNode
    }


diff :: ∀ msg. Node msg -> Node msg -> List (Patch msg)
diff a b = diffHelp a b Nil Nil


diffHelp :: ∀ msg. Node msg -> Node msg -> List (Patch msg) -> List Int -> List (Patch msg)
diffHelp a b patches index =
    -- Can't use regular equality because of the possible thunks ... should consider
    -- a workaround, like perhaps forcing the thunks to have unique tags that cn be
    -- compared in some way.
    if a `refEq` b
        then patches
        else
            case {a, b} of
                {a: Thunk aThunk, b: Thunk bThunk} ->
                    unsafeCrashWith "TODO"

                    {-
                    case 'thunk':
                        var aArgs = a.args;
                        var bArgs = b.args;
                        var i = aArgs.length;
                        var same = a.func === b.func && i === bArgs.length;
                        while (same && i--)
                        {
                            same = aArgs[i] === bArgs[i];
                        }
                        if (same)
                        {
                            b.node = a.node;
                            return;
                        }
                        b.node = b.thunk();
                        var subPatches = [];
                        diffHelp(a.node, b.node, subPatches, 0);
                        if (subPatches.length > 0)
                        {
                            patches.push(makePatch('p-thunk', index, subPatches));
                        }
                        return;
                    -}

                {a: Thunk2 aThunk, b: Thunk2 bThunk} ->
                    unsafeCrashWith "TODO"

                {a: Thunk3 aThunk, b: Thunk3 bThunk} ->
                    unsafeCrashWith "TODO"

                {a: Tagger aTagger, b: Tagger bTagger} ->
                    unsafeCrashWith "TODO"

                    {-
                    // gather nested taggers
                    var aTaggers = a.tagger;
                    var bTaggers = b.tagger;
                    var nesting = false;

                    var aSubNode = a.node;
                    while (aSubNode.type === 'tagger')
                    {
                        nesting = true;

                        typeof aTaggers !== 'object'
                            ? aTaggers = [aTaggers, aSubNode.tagger]
                            : aTaggers.push(aSubNode.tagger);

                        aSubNode = aSubNode.node;
                    }

                    var bSubNode = b.node;
                    while (bSubNode.type === 'tagger')
                    {
                        nesting = true;

                        typeof bTaggers !== 'object'
                            ? bTaggers = [bTaggers, bSubNode.tagger]
                            : bTaggers.push(bSubNode.tagger);

                        bSubNode = bSubNode.node;
                    }

                    // Just bail if different numbers of taggers. This implies the
                    // structure of the virtual DOM has changed.
                    if (nesting && aTaggers.length !== bTaggers.length)
                    {
                        patches.push(makePatch('p-redraw', index, b));
                        return;
                    }

                    // check if taggers are "the same"
                    if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
                    {
                        patches.push(makePatch('p-tagger', index, bTaggers));
                    }

                    // diff everything below the taggers
                    diffHelp(aSubNode, bSubNode, patches, index + 1);
                    return;
                    -}

                {a: Text aText, b: Text bText} ->
                    if aText == bText
                        then patches
                        else snoc patches (makePatch (PText bText) index)

                {a: PlainNode aNode, b: PlainNode bNode} ->
                    if aNode.tag /= bNode.tag || aNode.namespace /= bNode.namespace
                        then snoc patches (makePatch (PRedraw b) index)
                        else
                            let
                                factsDiff =
                                    diffFacts aNode.facts bNode.facts

                                patchesWithFacts =
                                    if Array.null factsDiff
                                        then patches
                                        else snoc patches (makePatch (PFacts factsDiff) index)

                            in
                                diffChildren aNode bNode patchesWithFacts index

                {a: Custom, b: Custom} ->
                    unsafeCrashWith "TODO"

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

                _ ->
                    -- This covers the case where they are different types
                    -- TODO: Probably shouldn't use `List`, since we're appending
                    snoc patches (makePatch (PRedraw b) index)


{-

// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

-}


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
                    singleton (AddEvent k options decoder)

        styleChanges =
            facts.styles #
                foldMap \k v ->
                    singleton (AddStyle k v)

        propertyChanges =
            facts.properties #
                foldMap \k v ->
                    singleton (AddProperty k v)


diffFacts :: ∀ msg. OrganizedFacts msg -> OrganizedFacts msg -> Array (FactChange msg)
diffFacts old new =
    runPure do
        runSTArray do
            -- I suppose the other alternative would be a Writer monad ... perhaps
            -- that would be better?
            accum <- emptySTArray

            let
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
                        Just newValue ->
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
                        Just newValue ->
                            -- We'll have done this one already ...
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveStyle k)

            -- Push removals first, then additions
            foldM oldAttribute unit old.attributes
            foldM oldAttributeNS unit old.attributesNS
            foldM oldStyle unit old.styles

            foldM newAttribute unit new.attributes
            foldM newAttributeNS unit new.attributesNS
            foldM newStyle unit new.styles

            -- TODO
            -- foldM newEvent unit new.events -- uses equalEvents
            -- foldM oldEvent unit old.events
            -- foldM newProperty unit new.properties -- uses reference equality, but always passes through "value"
            -- foldM oldProperty unit old.properties

            pure accum


diffChildren :: ∀ msg. NodeRecord msg -> NodeRecord msg -> List (Patch msg) -> List Int -> List (Patch msg)
diffChildren aParent bParent patches rootIndex =
    let
        aChildren = aParent.children
        bChildren = bParent.children

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
            zip aParent.children bParent.children

        diffPairs =
            foldl diffChild { subIndex: 0, patches: insertsAndRemovals } pairs

        diffChild memo (Tuple aChild bChild) =
            { subIndex: memo.subIndex + 1
            , patches: diffHelp aChild bChild memo.patches (snoc rootIndex memo.subIndex)
            }

    in
        diffPairs.patches


addDomNodes :: ∀ e msg. DOM.Node -> Node msg -> List (Patch msg) -> EventNode -> Eff (dom :: DOM | e) (List (PatchWithNodes msg))
addDomNodes rootNode vNode patches eventNode = do
    patches
        # List.foldM step
            { currentNode: rootNode
            , currentIndex: Nil
            , accum: Nil
            }
        <#> \result ->
            reverse result.accum

    where
        step params patch = do
            let
                fromRoot =
                    traversal Nil patch.index

                fromCurrent =
                    traversal params.currentIndex patch.index

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
                            { patch
                            , domNode
                            , eventNode
                            }

                    in
                        pure
                            { currentNode: domNode
                            , currentIndex: patch.index
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

applyPatches :: ∀ e msg. DOM.Node -> Node msg -> List (Patch msg) -> EventNode -> Eff (dom :: DOM | e) DOM.Node
applyPatches rootDomNode oldVirtualNode patches eventNode =
    if List.null patches
        then
            pure rootDomNode

        else
            addDomNodes rootDomNode oldVirtualNode patches eventNode
            >>= applyPatchesHelp rootDomNode


applyPatchesHelp :: ∀ e msg. DOM.Node -> List (PatchWithNodes msg) -> Eff (dom :: DOM | e) DOM.Node
applyPatchesHelp =
    List.foldM \rootNode patch -> do
        newNode <- applyPatch patch patch.domNode
        if patch.domNode `refEq` rootNode
            then pure newNode
            else pure rootNode


applyPatch :: ∀ e msg. PatchWithNodes msg -> DOM.Node -> Eff (dom :: DOM | e) DOM.Node
applyPatch patch domNode = do
    document <-
        documentForNode domNode

    case patch.patch.type_ of
        PRedraw vNode ->
            redraw domNode vNode patch.eventNode

        PFacts changes -> do
            nodeToElement domNode
                <#> applyFacts patch.eventNode changes
                # fromMaybe (pure unit)

            pure domNode

        PText string -> do
            setTextContent string domNode
            pure domNode

        PThunk ->
            pure domNode

        {-
			return applyPatchesHelp(domNode, patch.data);
        -}

        PTagger ->
            pure domNode

        {-
			domNode.elm_event_node_ref.tagger = patch.data;
			return domNode;
        -}

        PRemoveLast howMany -> do
            -- There must be a replicateM somewhere I'm forgetting ...
            forE 0 howMany \_ ->
                lastChild domNode <#> toMaybe >>=
                    case _ of
                        Just child -> do
                            removeChild child domNode
                            pure unit

                        Nothing ->
                            pure unit

            pure domNode

        PAppend newNodes -> do
            for_ newNodes \n ->
                render document n patch.eventNode
                >>= (flip appendChild) domNode

            pure domNode

        PCustom ->
		    pure domNode

        {-
        case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);
        -}


redraw :: ∀ e msg. DOM.Node -> Node msg -> EventNode -> Eff (dom :: DOM | e) DOM.Node
redraw domNode vNode eventNode = do
    document <- documentForNode domNode
    parentNode <- toMaybe <$> parentNode domNode
    newNode <- render document vNode eventNode
	
    {-
    if (typeof newNode.elm_event_node_ref === 'undefined')
 	{
+		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
    -}

    case parentNode of
        Just p -> do
            replaceChild newNode domNode p
            pure newNode

        Nothing ->
            pure newNode


{-

////////////  PROGRAMS  ////////////


function programWithFlags(details)
{
	return {
		init: details.init,
		update: details.update,
		subscriptions: details.subscriptions,
		view: details.view,
		renderer: renderer
	};
}

-}
