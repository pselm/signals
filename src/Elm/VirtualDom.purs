
-- | API to the core diffing algorithm. Can serve as a foundation for libraries
-- | that expose more helper functions for HTML or SVG.

module Elm.VirtualDom
    ( Node, text, node
    , Property, property, attribute, attributeNS, style
    , on, onWithOptions, Options, defaultOptions
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
    )

import Control.Monad.ST (pureST, newSTRef, writeSTRef, readSTRef)
import Control.Monad.Eff (Eff, runPure, forE)
import Control.Monad (unless)
import Unsafe.Coerce (unsafeCoerce)
import Partial (crashWith)

import Data.Array (null) as Array
import Data.Tuple (Tuple(..))
import Data.List (List(..), length, singleton, snoc, drop, zip)
import Data.Array.ST (runSTArray, emptySTArray, pushSTArray)
import Data.Foldable (class Foldable, foldl, for_)
import Data.StrMap (StrMap, foldM, foldMap, lookup)
import Data.StrMap.ST (new, poke, peek)
import Data.StrMap.ST.Unsafe (unsafeGet)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Exists (Exists, runExists, mkExists)
import Data.Foreign (readString)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Nullable (toNullable, toMaybe)

import DOM (DOM)
import DOM.Node.Node (appendChild, parentNode, replaceChild, lastChild, setTextContent, removeChild)
import DOM.Node.Types (Element, textToNode, elementToNode)
import DOM.Node.Types (Node) as DOM
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (setAttribute, removeAttribute)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument)

import Prelude (class Eq, Unit, unit, flip, (+), (-), void, pure, bind, (>>=), ($), (<$>), (<#>), (==), (/=), (||), (#), (<>), (<), (>))


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
    , descendantsCount :: Int
    }


newtype TaggerRecord msg sub = TaggerRecord
    { tagger :: sub -> msg
    , child :: Node sub
    , descendantsCount :: Int
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


descendants :: ∀ msg. Node msg -> Int
descendants n =
    case n of
        Text _ -> 0

        PlainNode {descendantsCount} ->
            descendantsCount

        Tagger tagger ->
            tagger #
                runExists \(TaggerRecord t) ->
                    t.descendantsCount

        Thunk _ -> 0
        Thunk2 _ -> 0
        Thunk3 _ -> 0

        -- TODO: Check if this is right
        Custom -> 0


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
        , descendantsCount
        , facts: organized.facts
        }

    where
        descendantsCount =
            (foldl (\memo n -> memo + (descendants n)) 0 children) + (length children)

        organized =
            organizeFacts properties


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
    Tagger (mkExists (TaggerRecord {tagger, child, descendantsCount}))

    where
        descendantsCount =
            descendants child + 1


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

render :: ∀ e msg. (Partial) => Node msg -> EventNode -> Eff (dom :: DOM | e) DOM.Node
render vNode eventNode = do
    doc <-
        -- TODO: The document should probably be handled via state?
        window
        >>= document
        <#> htmlDocumentToDocument

    case vNode of
        Thunk t ->
            crashWith "TODO"

{-
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);
-}

        Thunk2 t ->
            crashWith "TODO"

        Thunk3 t ->
            crashWith "TODO"

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
                renderedChild <- render child eventNode
                appendChild renderedChild (elementToNode domNode)

            pure (elementToNode domNode)

        Tagger rec ->
            crashWith "TODO"

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
            crashWith "TODO"

{-
		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
-}


-- APPLY FACTS

applyFacts :: ∀ e f msg. (Partial, Foldable f) => EventNode -> f (FactChange msg) -> Element -> Eff (dom :: DOM | e) Unit
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
                crashWith "TODO"

            RemoveEvent key options ->
                crashWith "TODO"

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
    | PRemove Int
    | PInsert (List (Node msg))
    | PCustom


type Patch msg =
    { index :: Int
    , type_ :: PatchOp msg
    }


type PatchWithNodes msg =
    { patch :: Patch msg
    , domNode :: DOM.Node
    , eventNode :: EventNode
    }


diff :: ∀ msg. (Partial) => Node msg -> Node msg -> List (Patch msg)
diff a b = diffHelp a b Nil 0


makePatch :: ∀ msg. PatchOp msg -> Int -> Patch msg
makePatch type_ index =
    { index
    , type_
    }


diffHelp :: ∀ msg. (Partial) => Node msg -> Node msg -> List (Patch msg) -> Int -> List (Patch msg)
diffHelp a b patches index =
    -- Can't use regular equality because of the possible thunks ... should consider
    -- a workaround, like perhaps forcing the thunks to have unique tags that cn be
    -- compared in some way.
    if a `refEq` b
        then patches
        else
            case {a, b} of
                {a: Thunk aThunk, b: Thunk bThunk} ->
                    crashWith "TODO"

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
                    crashWith "TODO"

                {a: Thunk3 aThunk, b: Thunk3 bThunk} ->
                    crashWith "TODO"

                {a: Tagger aTagger, b: Tagger bTagger} ->
                    crashWith "TODO"

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
                    crashWith "TODO"

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

            foldM newAttribute unit new.attributes
            foldM oldAttribute unit old.attributes
            foldM newAttributeNS unit new.attributesNS
            foldM oldAttributeNS unit old.attributesNS
            foldM newStyle unit new.styles
            foldM oldStyle unit old.styles
            -- TODO
            -- foldM newEvent unit new.events -- uses equalEvents
            -- foldM oldEvent unit old.events
            -- foldM newProperty unit new.properties -- uses reference equality, but always passes through "value"
            -- foldM oldProperty unit old.properties

            pure accum


diffChildren :: ∀ msg. (Partial) => NodeRecord msg -> NodeRecord msg -> List (Patch msg) -> Int -> List (Patch msg)
diffChildren aParent bParent patches rootIndex =
    let
        aChildren = aParent.children
        bChildren = bParent.children

        aLen = length aChildren
        bLen = length bChildren

        insertsAndRemovals =
            if aLen > bLen
                then snoc patches (makePatch (PRemove (aLen - bLen)) rootIndex)
                else
                    if aLen < bLen
                        then snoc patches (makePatch (PInsert (drop aLen bChildren)) rootIndex)
                        else patches

        pairs =
            zip aParent.children bParent.children

        diffPairs =
            foldl diffChild { index: rootIndex, patches: insertsAndRemovals } pairs

        diffChild memo (Tuple aChild bChild) =
            { index: memo.index + 1 + (descendants aChild)
            , patches: diffHelp aChild bChild memo.patches (memo.index + 1)
            }

    in
        diffPairs.patches




-- ADD DOM NODES
--
-- Each DOM node has an "index" assigned in order of traversal. It is important
-- to minimize our crawl over the actual DOM, so these indexes (along with the
-- descendantsCount of virtual nodes) let us skip touching entire subtrees of
-- the DOM if we know there are no patches there.

{-
function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			return addDomNodesHelp(domNode, vNode.node, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

-}


applyPatch :: ∀ e msg. (Partial) => PatchWithNodes msg -> DOM.Node -> Eff (dom :: DOM | e) DOM.Node
applyPatch patch domNode =
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

        PRemove howMany -> do
            forE 0 howMany \_ ->
                lastChild domNode <#> toMaybe >>=
                    case _ of
                        Just child -> do
                            removeChild child domNode
                            pure unit

                        Nothing ->
                            pure unit

            pure domNode

        PInsert newNodes -> do
            for_ newNodes \n ->
                render n patch.eventNode
                >>= (flip appendChild) domNode

            pure domNode

        PCustom ->
		    pure domNode

        {-
        case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);
        -}


redraw :: ∀ e msg. (Partial) => DOM.Node -> Node msg -> EventNode -> Eff (dom :: DOM | e) DOM.Node
redraw domNode vNode eventNode = do
    parentNode <- toMaybe <$> parentNode domNode
    newNode <- render vNode eventNode
	
    {-
    var ref = domNode.elm_event_node_ref
	if (typeof ref !== 'undefined')
	{
		newNode.elm_event_node_ref = ref;
	}
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
