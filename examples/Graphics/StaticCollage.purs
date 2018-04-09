
-- | Testing `Collage` by itself ... just rendering, no updating.
-- |
-- | For instructions on building and running, see StaticCollage.html in this
-- | directory.

module Examples.Graphics.StaticCollage where


import Examples.Graphics.CollageExamples (examples)

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS)
import Data.Foldable (for_)

import DOM (DOM)
import DOM.Renderable (renderIntoDOM, Position(AfterLastChild))
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..), textToNode, documentToNonElementParentNode)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Node (appendChild)

import Prelude (Unit, ($), void, bind, discard, (<$>), (>>=))


main :: ∀ e. Eff (canvas :: CANVAS, dom :: DOM | e) Unit
main = do
    doc <-
        htmlDocumentToDocument <$>
            (window >>= document)

    nullableContainer <-
        getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    for_ nullableContainer \container -> do
        table <- elementToNode <$> createElement "table" doc
        tbody <- elementToNode <$> createElement "tbody" doc
        thead <- elementToNode <$> createElement "thead" doc

        void $ appendChild thead table
        void $ appendChild tbody table
        void $ appendChild table (elementToNode container)

        column1 <- elementToNode <$> createElement "th" doc
        column2 <- elementToNode <$> createElement "th" doc
        column3 <- elementToNode <$> createElement "th" doc
        column4 <- elementToNode <$> createElement "th" doc

        text1 <- textToNode <$> createTextNode "Code" doc
        text2 <- textToNode <$> createTextNode "Result" doc
        text3 <- textToNode <$> createTextNode "Should look like" doc
        text4 <- textToNode <$> createTextNode "Difference" doc

        void $ appendChild text1 column1
        void $ appendChild text2 column2
        void $ appendChild text3 column3
        void $ appendChild text4 column4

        void $ appendChild column1 thead
        void $ appendChild column2 thead
        void $ appendChild column3 thead
        void $ appendChild column4 thead

        for_ examples $
            renderIntoDOM AfterLastChild tbody

