module Test.Elm.Graphics.StaticElementTest (tests) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.JSDOM (JSDOM, jsdom)
import DOM.Node.Document (createElement)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Document, elementToNode)
import DOM.Renderable (render)
import Data.Foldable (for_)
import Data.List (fromFoldable, filter)
import Data.Maybe (Maybe(..), isJust)
import Elm.Graphics.Element (flow, down)
import Examples.Graphics.StaticElement (Expectation, expectations)
import Graphics.Canvas (CANVAS)
import Prelude (class Show, class Eq, flip, bind, discard, void, ($), (#))
import Test.Elm.Graphics.Internal (innerHtml)
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)


infixl 9 equals as ===

equals :: forall a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


makeDocument :: ∀ e. Eff (dom :: DOM, jsdom :: JSDOM | e) Document
makeDocument =
    jsdom "" {}


tests :: ∀ e. TestSuite (canvas :: CANVAS, dom :: DOM, err :: EXCEPTION, jsdom :: JSDOM | e)
tests = do
    suite "Elm.Graphics.StaticElement" $
        for_ (filter hasExpected (fromFoldable expectations)) makeTest

    where
        hasExpected {expected} =
            isJust expected


makeTest :: ∀ e. Expectation -> TestSuite (canvas :: CANVAS, dom :: DOM, err :: EXCEPTION, jsdom :: JSDOM | e)
makeTest expectation =
    test expectation.title do
        document <-
            liftEff $ makeDocument

        wrapper <-
            liftEff $ createElement "div" document

        child <-
            flow down (fromFoldable expectation.elements)
            # render document
            # liftEff

        liftEff $ void $ appendChild child (elementToNode wrapper)

        result <-
            liftEff $ innerHtml wrapper

        Just result === expectation.expected
