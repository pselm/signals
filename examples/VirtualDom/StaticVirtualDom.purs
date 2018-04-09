module Examples.VirtualDom.StaticVirtualDom where

import Elm.VirtualDom

import Control.Monad.Eff (Eff)

import DOM (DOM)
import DOM.Renderable (render)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)
import Graphics.Canvas (CANVAS)

import Data.Foldable (for_)
import Data.List (List(..), (:))

import Prelude (bind, Unit, (>>=))


main :: Eff (canvas :: CANVAS, dom :: DOM) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ nullableContainer \container -> do
        element <- render (htmlDocumentToDocument doc) scene
        appendChild element (elementToNode container)


scene :: ∀ msg. Node msg
scene =
    node "p"
        Nil
        ( text "Hello World!"
        : Nil
        )


