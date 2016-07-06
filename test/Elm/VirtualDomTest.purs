module Test.Elm.VirtualDomTest (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

import Test.Internal.Graphics (innerHtml)

import Elm.VirtualDom

import DOM (DOM)
import DOM.Renderable (Position(..), renderIntoDOM)
import DOM.JSDOM (JSDOM, jsdom)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)

import Graphics.Canvas (CANVAS)
import Data.Nullable (toMaybe)
import Data.Maybe (fromJust)
import Data.List (List(..), (:))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Partial.Unsafe (unsafePartial)

import Prelude (class Show, class Eq, flip, bind, ($), (<#>), (>>>), pure)


infixl 9 equals as ===

equals :: forall a e. (Eq a, Show a) => a -> a -> Test e
equals = flip equal


blank :: String
blank =
    """
    <html>
        <head></head>
        <body>
            <div id="contents">
            </div>
        </body>
    </html>
    """


withContainer :: ∀ e a. (Element -> Eff (jsdom :: JSDOM, dom :: DOM | e) a) -> Eff (dom :: DOM, jsdom :: JSDOM | e) a
withContainer callback = do
    doc <-
        jsdom blank {}

    node <-
        getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    -- Should be safe, because the HTML is just above
    unsafePartial (callback (fromJust (toMaybe node)))


tests :: forall e. TestSuite (canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
tests =
    suite "Elm.VirtualDom" do
        test "Hello World" do
            container <-
                liftEff $
                    withContainer \c -> do
                        renderIntoDOM ReplacingChildren (elementToNode c) helloWorld
                        pure c

            result <-
                liftEff $ innerHtml container

            result === helloWorldResult


helloWorldResult :: String
helloWorldResult = "<p>Hello World!</p>"

helloWorld :: ∀ msg. Node msg
helloWorld =
    node "p"
        Nil
        ( text "Hello World!"
        : Nil
        )
