
-- | Some helper functions used internally by multiple modules.
-- | Not part of the official API, thus subject to change without affecting semver.

module Elm.Graphics.Internal
    ( createNode
    , removePaddingAndMargin
    , setStyle, removeStyle
    , addTransform, removeTransform
    , getDimensions, measure
    , setProperty, getProperty, removeProperty, setPropertyIfDifferent
    , setAttributeNS, getAttributeNS, removeAttributeNS
    , defaultView
    , nodeToElement, documentToHtmlDocument
    , nextEventTarget
    , documentForNode
    , EventHandler, eventHandler, addEventHandler, setHandlerInfo, removeEventHandler
    , makeCustomEvent, eventToCustomEvent, detail
    ) where


import Control.Comonad (extract)
import Control.Monad.Eff (Eff, foreachE, kind Effect)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM (DOM)
import DOM.Event.Types (CustomEvent, Event, EventTarget, EventType, readCustomEvent, readEventTarget)
import DOM.HTML.Document (body)
import DOM.HTML.Types (Window, HTMLDocument, htmlElementToNode, readHTMLDocument)
import DOM.Node.Document (createElement)
import DOM.Node.Node (appendChild, removeChild, nextSibling, insertBefore, parentNode, nodeType, ownerDocument)
import DOM.Node.NodeType (NodeType(ElementNode))
import DOM.Node.Types (Document, Element, Node, elementToNode)
import Data.Either (Either(..), either, hush)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, const, discard, pure, void, ($), (<#>), (<$>), (<<<), (>>=))
import Unsafe.Coerce (unsafeCoerce)


-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: ∀ e. String -> String -> Element -> Eff (dom :: DOM | e) Unit


-- Removes the style
foreign import removeStyle :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Unit


-- Dimensions
foreign import getDimensions :: ∀ e. Element -> Eff (dom :: DOM | e) {width :: Number, height :: Number}


-- Set arbitrary property. TODO: Should suggest for purescript-dom
foreign import setProperty :: ∀ e. String -> Foreign -> Element -> Eff (dom :: DOM | e) Unit

-- Get arbitrary property.
foreign import getProperty :: ∀ e. String -> Element -> Eff (dom :: DOM | e) (Nullable Foreign)

-- Remove a property.
foreign import removeProperty :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Unit

-- Set if not already equal. A bit of a hack ... not suitable for general use.
foreign import setPropertyIfDifferent :: ∀ e. String -> Foreign -> Element -> Eff (dom :: DOM | e) Unit


-- TODO: Should suggest these for purescript-dom
foreign import setAttributeNS :: ∀ e. String -> String -> String -> Element -> Eff (dom :: DOM | e) Unit
foreign import getAttributeNS :: ∀ e. String -> String -> Element -> Eff (dom :: DOM | e) (Nullable String)
foreign import removeAttributeNS :: ∀ e. String -> String -> Element -> Eff (dom :: DOM | e) Unit

foreign import defaultView :: HTMLDocument -> Nullable Window


-- | Given a node, returns the document which the node belongs to.
documentForNode :: ∀ e. Node -> Eff (dom :: DOM | e) Document
documentForNode node =
    -- The unsafeCoerce should be safe, because if `ownerDocument`
    -- returns null, then the node itself must be the document.
    ownerDocument node
        <#> fromMaybe (unsafeCoerce node)


createNode :: ∀ e. Document -> String -> Eff (dom :: DOM | e) Element
createNode document elementType = do
    node <-
        createElement elementType document

    removePaddingAndMargin node
    pure node


removePaddingAndMargin :: ∀ e. Element -> Eff (dom :: DOM | e) Unit
removePaddingAndMargin elem =
    foreachE
        [ setStyle "padding" "0px"
        , setStyle "margin" "0px"
        ] \op -> op elem


vendorTransforms :: Array String
vendorTransforms =
    [ "transform"
    , "msTransform"
    , "MozTransform"
    , "webkitTransform"
    , "OTransform"
    ]


addTransform :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Unit
addTransform transform node =
    foreachE vendorTransforms \t ->
        setStyle t transform node


removeTransform :: ∀ e. Element -> Eff (dom :: DOM | e) Unit
removeTransform node =
    foreachE vendorTransforms \t ->
        removeStyle t node


-- Note that if the node is already in a document, you can just run getDimensions.
-- This is effectful, in the sense that the node will be removed from any parent
-- it currently has (though we will put it back at the end).
measure :: ∀ e. Node -> Eff (dom :: DOM | e) {width :: Number, height :: Number}
measure node = do
    maybeHtmlDoc <-
        documentToHtmlDocument <$> documentForNode node

    maybeBody <-
        case maybeHtmlDoc of
            Just doc ->
                body doc

            Nothing ->
                pure Nothing

    case maybeBody of
        Just b -> do
            doc <-
                documentForNode node

            temp <-
                createElement "div" doc

            setStyle "visibility" "hidden" temp
            setStyle "float" "left" temp

            oldSibling <- nextSibling node
            oldParent <- parentNode node

            void $ appendChild node (elementToNode temp)

            let bodyDoc = htmlElementToNode b
            void $ appendChild (elementToNode temp) bodyDoc

            dim <- getDimensions temp

            void $ removeChild (elementToNode temp) bodyDoc

            -- Now, we should put it back ...
            case oldParent of
                Just p ->
                    case oldSibling of
                        Just s ->
                            void $ insertBefore node s p

                        Nothing ->
                            void $ appendChild node p

                Nothing ->
                    void $ removeChild node (elementToNode temp)

            pure dim

        Nothing ->
            pure
                { width: 0.0
                , height: 0.0
                }


unsafeNodeToElement :: Node -> Element
unsafeNodeToElement = unsafeCoerce


-- Perhaps should suggest this for purescript-dom?
nodeToElement :: Node -> Maybe Element
nodeToElement node =
    unsafePartial
        case nodeType node of
            ElementNode ->
                Just (unsafeNodeToElement node)

            _ ->
                Nothing


documentToHtmlDocument :: Document -> Maybe HTMLDocument
documentToHtmlDocument doc =
    extract $ either (const Nothing) Just <$>
        runExceptT (readHTMLDocument (toForeign doc))


-- | Like `DOM.Event.EventTarget.EventListener`, but parameterized by the `msg`
-- | type, and you can mutate the "info" that it uses without removing and
-- | adding the listener.
foreign import data EventHandler :: # Effect -> Type -> Type


-- | Like `DOM.Event.EventTarget.eventListener`, but your function also gets
-- | some info of type `i`, and you can change that info without removing and
-- | re-applying the handler. And, since you can mutate the result, producing
-- | it needs to be an `Eff` itself.
-- |
-- | You have to supply some initial info (which you can change using
-- | `setHandlerInfo`).
foreign import eventHandler :: ∀ e i a.
    (i -> Event -> Eff e a) -> i -> Eff e (EventHandler e i)


-- | Like `addEventListener`, but for handlers. The `Boolean` arg indicates
-- | whether to use the "capture" phase.
foreign import addEventHandler :: ∀ e i.
    EventType ->
    EventHandler (dom :: DOM | e) i ->
    Boolean ->
    EventTarget ->
    Eff (dom :: DOM | e) Unit


-- | Supply a different value for the handler, for use with the callback
-- | function, without removing and re-applying the handler.
foreign import setHandlerInfo :: ∀ e1 e2 i.
    i -> EventHandler e1 i -> Eff (dom :: DOM | e2) Unit


-- | Like `removeEventListener`
foreign import removeEventHandler :: ∀ e i.
    EventType ->
    EventHandler (dom :: DOM | e) i ->
    Boolean ->
    EventTarget ->
    Eff (dom :: DOM | e) Unit


-- | It feels as though purescript-dom ought to have a way to do this, but I
-- | can't find it. Stores the `Foreign` in the `detail` field of the custom
-- | event.
foreign import makeCustomEvent :: ∀ e. EventType -> Foreign -> Eff e CustomEvent


eventToCustomEvent :: Event -> Maybe CustomEvent
eventToCustomEvent = hush <<< runExcept <<< readCustomEvent <<< toForeign


foreign import _detail :: CustomEvent -> Nullable Foreign


detail :: CustomEvent -> Maybe Foreign
detail = toMaybe <<< _detail


nextEventTarget :: ∀ e. Node -> Eff (dom :: DOM | e) (Maybe EventTarget)
nextEventTarget =
    tailRecM \node ->
        parentNode node >>= case _ of
            Just parent ->
                case runExcept $ readEventTarget $ toForeign parent of
                    Right eventTarget ->
                        pure $ Done $ Just eventTarget

                    Left _ ->
                        pure $ Loop parent

            Nothing ->
                pure $ Done $ Nothing
