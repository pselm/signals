
-- | Some helper functions used internally by multiple modules.
-- | Not part of the official API, thus subject to change without affecting semver.

module Elm.Graphics.Internal
    ( createNode
    , removePaddingAndMargin
    , setStyle, removeStyle
    , addTransform, removeTransform
    , getDimensions, measure
    , setProperty, removeProperty, setPropertyIfDifferent
    , setAttributeNS, getAttributeNS, removeAttributeNS
    , defaultView
    , nodeToElement, documentToHtmlDocument
    , documentForNode
    ) where


import DOM (DOM)
import DOM.HTML.Types (Window, HTMLDocument, htmlElementToNode, readHTMLDocument)
import DOM.HTML.Document (body)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Document, Element, Node, elementToNode)
import DOM.Node.NodeType (NodeType(ElementNode))
import DOM.Node.Node (appendChild, removeChild, nextSibling, insertBefore, parentNode, nodeType, ownerDocument)
import Data.Nullable (Nullable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (either)
import Data.Foreign (Foreign, toForeign)
import Control.Comonad (extract)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Except.Trans (runExceptT)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (bind, discard, pure, void, Unit, (<#>), (<$>), ($), const)


-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: ∀ e. String -> String -> Element -> Eff (dom :: DOM | e) Unit


-- Removes the style
foreign import removeStyle :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Unit


-- Dimensions
foreign import getDimensions :: ∀ e. Element -> Eff (dom :: DOM | e) {width :: Number, height :: Number}


-- Set arbitrary property. TODO: Should suggest for purescript-dom
foreign import setProperty :: ∀ e. String -> Foreign -> Element -> Eff (dom :: DOM | e) Unit

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
