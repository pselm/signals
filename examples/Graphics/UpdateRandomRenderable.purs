
-- | Testing updating a random `Renderable` where the `Renderable` sometimes has
-- | different underlying types, so that sometimes a complete re-render should be
-- | triggered.
-- |
-- | For instructions on building and running, see UpdateRandomRenderable.html in this
-- | directory.

module Examples.Graphics.UpdateRandomRenderable where


import Examples.Graphics.CollageExamples (collages) as Examples
import Examples.Graphics.StaticElement (elements) as Examples

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.ST (newSTRef, readSTRef, writeSTRef, runST)
import Graphics.Canvas (CANVAS)
import Data.NonEmpty (NonEmpty(..))
import Data.Foldable (for_)
import Data.Tuple (fst, snd)
import Test.QuickCheck.LCG (randomSeed)
import Test.QuickCheck.Gen (Gen, runGen, elements)

import DOM (DOM)
import DOM.Renderable (AnyRenderable, toAnyRenderable, renderOrUpdate)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.HTML.Event.EventTypes (keydown)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode, documentToEventTarget)
import DOM.Event.EventTarget (eventListener, addEventListener)

import Prelude (Unit, bind, discard, (<$>), (>>=), const, (<>), ($), void)


-- This could be made more general with some classes ... in fact, I would be
-- surprised if it didn't exist already.
appendNonEmpty :: ∀ a. NonEmpty Array a -> NonEmpty Array a -> NonEmpty Array a
appendNonEmpty (NonEmpty a as) (NonEmpty b bs) =
    NonEmpty a $ as <> [b] <> bs


-- Make a list of all the collages and all the elements, together
collagesAndElements :: NonEmpty Array AnyRenderable
collagesAndElements =
   appendNonEmpty 
        (toAnyRenderable <$> Examples.collages)
        (toAnyRenderable <$> Examples.elements)


genAnyRenderable :: Gen AnyRenderable
genAnyRenderable =
    elements collagesAndElements


main :: ∀ e. Eff (canvas :: CANVAS, dom :: DOM, random :: RANDOM | e) Unit
main = do
    doc <-
        htmlDocumentToDocument <$>
            (window >>= document)

    nullableContainer <-
        getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    for_ nullableContainer \container -> do
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
                    let result = runGen genAnyRenderable randomState
                    void $ writeSTRef randomRef (snd result)
                    renderOrUpdate container (fst result)

                listener =
                    eventListener (const doUpdate)

            addEventListener keydown listener false (documentToEventTarget doc)

            doUpdate
