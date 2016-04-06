
module Elm.Graphics.Internal
    ( createNode, setStyle, removeStyle
    ) where


import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToDocument, htmlElementToNode)
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


