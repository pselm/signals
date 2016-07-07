module Test.Elm.VirtualDomTest (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

import Test.Elm.Graphics.Internal (innerHtml)

import Elm.VirtualDom

import DOM (DOM)
import DOM.Renderable (Position(..), renderIntoDOM)
import DOM.JSDOM (JSDOM, jsdom)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)

import Elm.Json.Encode as Json
import Graphics.Canvas (CANVAS)
import Data.Nullable (toMaybe)
import Data.Maybe (fromJust)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
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
    <html xmlns:ns="http://localhost/bob">
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
        makeTest helloWorld
        makeTest propertyTest
        makeTest attributeTest
        makeTest attributeNsTest
        makeTest styleTest
        makeTest namespacedNode


makeTest :: ∀ e msg. NodeTest msg -> TestSuite (canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
makeTest nodeTest =
    test nodeTest.title do
        container <-
            liftEff $
                withContainer \c -> do
                    renderIntoDOM ReplacingChildren (elementToNode c) nodeTest.node
                    pure c

        result <-
            liftEff $ innerHtml container

        result === nodeTest.expected


type NodeTest msg =
    { title :: String
    , node :: Node msg
    , expected :: String
    }


helloWorld :: ∀ msg. NodeTest msg
helloWorld =
    { title: "Simple <p> with text"
    , node:
        node "p"
            Nil
            ( text "Hello World!"
            : Nil
            )
    , expected:
        "<p>Hello World!</p>"
    }


propertyTest :: ∀ msg. NodeTest msg
propertyTest =
    { title: "property"
    , node:
        node "div"
            ( property "className" (Json.string "greeting")
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div class=\"greeting\">Hello!</div>"
    }


attributeTest :: ∀ msg. NodeTest msg
attributeTest =
    { title: "attribute"
    , node:
        node "div"
            ( attribute "class" "greeting"
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div class=\"greeting\">Hello!</div>"
    }


attributeNsTest :: ∀ msg. NodeTest msg
attributeNsTest =
    { title: "attributeNS"
    , node:
        node "div"
            ( attributeNS "ns" "data" "greeting"
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        -- JSDOM doesn't seem to put the namespace in the innerHTML ...
        "<div data=\"greeting\">Hello!</div>"
    }


styleTest :: ∀ msg. NodeTest msg
styleTest =
    { title: "style"
    , node:
        node "div"
            ( style
                ( Tuple "backgroundColor" "red"
                : Tuple "height" "90px"
                : Tuple "width" "100%"
                : Nil
                )
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div style=\"background-color: red; height: 90px; width: 100%;\">Hello!</div>"
    }


namespacedNode :: ∀ msg. NodeTest msg
namespacedNode =
    { title: "Namespaced node"
    , node:
        node "p"
            ( property "namespace" (Json.string "ns")
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        -- JSDOM doesn't seem to put the namespaces in the HTML.
        "<p>Hello!</p>"
    }
