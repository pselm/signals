
-- | Some helper functions used internally by multiple modules.
-- | Not part of the official API, thus subject to change without affecting semver.

module Elm.Graphics.Internal
    ( createNode
    , setStyle, removeStyle
    , addTransform, removeTransform
    ) where


import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Element) as DOM
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, (>>=), (>>>), pure)


-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: ∀ e. String -> String -> DOM.Element -> Eff (dom :: DOM | e) Unit


-- Removes the style
foreign import removeStyle :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit


createNode :: ∀ e. String -> Eff (dom :: DOM | e) DOM.Element
createNode elementType = do
    node <- window >>= document >>= htmlDocumentToDocument >>> createElement elementType
    setStyle "padding" "0px" node
    setStyle "margin" "0px" node
    pure node


addTransform :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit
addTransform transform node = do
    setStyle "transform" transform node
    setStyle "msTransform" transform node
    setStyle "MozTransform" transform node
    setStyle "webkitTransform" transform node
    setStyle "OTransform" transform node


removeTransform :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) Unit
removeTransform node = do
    removeStyle "transform" node
    removeStyle "msTransform" node
    removeStyle "MozTransform" node
    removeStyle "webkitTransform" node
    removeStyle "OTransform" node
