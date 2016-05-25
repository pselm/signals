
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
    ) where


import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToNode)
import DOM.HTML.Document (body)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Element, Node, elementToNode)
import DOM.Node.Node (appendChild, removeChild, nextSibling, insertBefore, parentNode)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.Foreign (Foreign)
import Control.Monad.Eff (Eff, foreachE)
import Prelude (bind, (>>=), (>>>), pure, Unit)


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


createNode :: ∀ e. String -> Eff (dom :: DOM | e) Element
createNode elementType = do
    node <-
        window >>=
        document >>=
        htmlDocumentToDocument >>>
        createElement elementType

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
-- it currently has.
measure :: ∀ e. Node -> Eff (dom :: DOM | e) {width :: Number, height :: Number}
measure node = do
    doc <-
        window >>= document

    nullableBody <-
        body doc

    case toMaybe nullableBody of
        Just b -> do
            temp <-
                createElement "div" (htmlDocumentToDocument doc)

            setStyle "visibility" "hidden" temp
            setStyle "float" "left" temp

            oldSibling <- nextSibling node
            oldParent <- parentNode node

            appendChild node (elementToNode temp)

            let bodyDoc = htmlElementToNode b
            appendChild (elementToNode temp) bodyDoc

            dim <- getDimensions temp

            removeChild (elementToNode temp) bodyDoc

            -- Now, we should put it back ...
            case toMaybe oldParent of
                Just p ->
                    case toMaybe oldSibling of
                        Just s ->
                            insertBefore node s p

                        Nothing ->
                            appendChild node p

                Nothing ->
                    removeChild node (elementToNode temp)

            pure dim

        Nothing ->
            pure
                { width: 0.0
                , height: 0.0
                }
