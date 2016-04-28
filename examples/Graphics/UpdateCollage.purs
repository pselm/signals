
-- | Testing `Collage` by itself ... with updating.
-- |
-- | For instructions on building and running, see UpdateCollage.html in this
-- | directory.

module Examples.Graphics.UpdateCollage where


import Examples.Graphics.CollageExamples (Example)

import Elm.Graphics.Collage

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.ST (newSTRef, readSTRef, writeSTRef, runST)
import Graphics.Canvas (Canvas)
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.Tuple (fst, snd)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.LCG (randomSeed)
import Test.QuickCheck.Gen (Gen, runGen)

import DOM (DOM)
import DOM.Renderable (renderOrUpdate)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..), textToNode, documentToNonElementParentNode, documentToEventTarget)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Node (appendChild)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (keydown)

import Prelude (Unit, bind, (<$>), (>>=), const)


main :: ∀ e. Eff (canvas :: Canvas, dom :: DOM, random :: RANDOM | e) Unit
main = do
    doc <-
        htmlDocumentToDocument <$>
            (window >>= document)

    nullableContainer <-
        getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        table <- elementToNode <$> createElement "table" doc
        tbody <- createElement "tbody" doc
        thead <- elementToNode <$> createElement "thead" doc

        appendChild thead table
        appendChild (elementToNode tbody) table
        appendChild table (elementToNode container)

        column1 <- elementToNode <$> createElement "th" doc
        column2 <- elementToNode <$> createElement "th" doc
        column3 <- elementToNode <$> createElement "th" doc
        column4 <- elementToNode <$> createElement "th" doc

        text1 <- textToNode <$> createTextNode "Code" doc
        text2 <- textToNode <$> createTextNode "Result" doc
        text3 <- textToNode <$> createTextNode "Should look like" doc
        text4 <- textToNode <$> createTextNode "Difference" doc

        appendChild text1 column1
        appendChild text2 column2
        appendChild text3 column3
        appendChild text4 column4

        appendChild column1 thead
        appendChild column2 thead
        appendChild column3 thead
        appendChild column4 thead

        seed <-
            randomSeed

        runST do
            randomRef <-
                newSTRef
                    { newSeed: seed
                    , size: 1
                    }

            let
                doUpdate = do
                    randomState <- readSTRef randomRef
                    let result = runGen (arbitrary :: Gen Example) randomState
                    writeSTRef randomRef (snd result)
                    renderOrUpdate tbody (fst result)

                listener =
                    eventListener (const doUpdate)

            addEventListener keydown listener false (documentToEventTarget doc)

            doUpdate
