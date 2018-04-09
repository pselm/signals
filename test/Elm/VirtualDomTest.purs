module Test.Elm.VirtualDomTest (tests) where

import Test.Unit (TestSuite, Test, suite, test, success, failure)
import Test.Unit.Assert (equal)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.LCG (randomSeed)
import Test.QuickCheck.Gen (GenState, elements, runGen)

import Test.Elm.Graphics.Internal (innerHtml)

import Elm.VirtualDom

import DOM (DOM)
import DOM.Renderable (Position(..), renderIntoDOM, renderOrUpdate)
import DOM.JSDOM (JSDOM, jsdom)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)

import Elm.Json.Encode as Json
import Graphics.Canvas (CANVAS)
import Data.Maybe (Maybe(..), fromJust)
import Data.List (List(..), (:), toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (traverse_)
import Data.NonEmpty (NonEmpty(..))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (Step(..), tailRecM3)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

import Prelude (class Show, class Eq, flip, bind, discard, void, ($), (<>), show, pure, unit, (<=), (==), (/=), (-))


infixl 9 equals as ===

equals :: forall a e. Eq a => Show a => a -> a -> Test e
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


withContainer :: ∀ e m a. (MonadEff (jsdom :: JSDOM, dom :: DOM | e) m) => (Element -> m a) -> m a
withContainer callback = do
    node <-
        liftEff do
            doc <- jsdom blank {}
            getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    -- Should be safe, because the HTML is just above
    unsafePartial (callback (fromJust node))


tests :: ∀ e. TestSuite (random :: RANDOM, canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
tests =
    suite "Elm.VirtualDom" do
        suite "Static Tests" $
            traverse_ makeStaticTest nodeTests

        specificTransitions
        randomTransitions


specificTransitions :: ∀ e. TestSuite (random :: RANDOM, canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
specificTransitions =
    test "Specific Transitions" do
        makeTransitionTest attributeTest helloWorld
        makeTransitionTest propertyTest attributeNsTest


randomTransitions :: ∀ e. TestSuite (random :: RANDOM, canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
randomTransitions =
    test "Random Transitions" $
        withContainer \container -> do
            seed <-
                liftEff $ randomSeed

            runFor container 250
                { newSeed: seed
                , size: 1
                } Nothing


makeUpdateTest :: ∀ e msg. Element -> Int -> NodeTest msg -> Maybe (NodeTest msg) -> Test (canvas :: CANVAS, dom :: DOM | e)
makeUpdateTest container remaining (NodeTest nodeTest) previous = do
    result <-
        liftEff do
            renderOrUpdate container nodeTest.node
            innerHtml container

    if result == nodeTest.expected
        then success
        else
            let
                context =
                    case previous of
                        Nothing ->
                            "on first render"

                        Just (NodeTest p) ->
                            "from " <> p.title

            in
                failure $
                    "expected " <> show nodeTest.expected <>
                    ", got " <> show result <>
                    ", for " <> nodeTest.title <>
                    ", " <> context <>
                    ", with remaining " <> show remaining


runFor :: ∀ e msg. Element -> Int -> GenState -> Maybe (NodeTest msg) -> Test (canvas :: CANVAS, dom :: DOM | e)
runFor container =
    tailRecM3 \remaining genState previous ->
        if remaining <= 0
            then
                pure $ Done unit

            else do
                let
                    state =
                        runGen arbitrary genState

                makeUpdateTest container remaining (fst state) previous

                pure $ Loop
                    { a: remaining - 1
                    , b: snd state
                    , c: Just (fst state)
                    }


makeTransitionTest :: ∀ e msg. NodeTest msg -> NodeTest msg -> Test (canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
makeTransitionTest (NodeTest from) (NodeTest to) =
    withContainer \container -> do
        resultFrom <-
            liftEff do
                renderOrUpdate container from.node
                innerHtml container

        if resultFrom /= from.expected
            then
                failure $
                    from.title <> " --> " <> to.title <>
                    " failed at from: expected " <> show from.expected <>
                    " actual " <> show resultFrom

            else do
                resultTo <-
                    liftEff do
                        renderOrUpdate container to.node
                        innerHtml container

                if resultTo == to.expected
                    then success
                    else
                        failure $
                            from.title <> " --> " <> to.title <>
                            " failed at to expected " <> show to.expected <>
                            " actual " <> show resultTo


makeStaticTest :: ∀ e msg. NodeTest msg -> TestSuite (canvas :: CANVAS, dom :: DOM, jsdom :: JSDOM | e)
makeStaticTest (NodeTest nodeTest) =
    test nodeTest.title do
        container <-
            liftEff $
                withContainer \c -> do
                    void $ renderIntoDOM ReplacingChildren (elementToNode c) nodeTest.node
                    pure c

        result <-
            liftEff $ innerHtml container

        result === nodeTest.expected


newtype NodeTest msg = NodeTest (NodeTestRec msg)


type NodeTestRec msg =
    { title :: String
    , node :: Node msg
    , expected :: String
    }


instance arbitraryNodeTest :: Arbitrary (NodeTest msg) where
    arbitrary =
        case nodeTests of
            Cons first rest ->
                elements $ NonEmpty first (toUnfoldable rest)

            _ ->
                unsafeCrashWith "Can't get here, because it's not an empty list"


nodeTests :: ∀ msg. List (NodeTest msg)
nodeTests =
    ( helloWorld : helloWorld2 : helloWorld3 : helloWorld4 : helloWorld5
    : propertyTest : propertyTest2
    : attributeTest : attributeTest2 : attributeTest3
    : styleTest
    : attributeNsTest
    : namespacedNode
    : custom1 : custom2
    : Nil
    )


helloWorld :: ∀ msg. NodeTest msg
helloWorld = NodeTest
    { title: "helloWorld"
    , node:
        node "p"
            Nil
            ( text "Hello World!"
            : Nil
            )
    , expected:
        "<p>Hello World!</p>"
    }


helloWorld2 :: ∀ msg. NodeTest msg
helloWorld2 = NodeTest
    { title: "helloWorld2"
    , node:
        node "p"
            Nil
            ( text "Hello to the World!"
            : Nil
            )
    , expected:
        "<p>Hello to the World!</p>"
    }


helloWorld3 :: ∀ msg. NodeTest msg
helloWorld3 = NodeTest
    { title: "helloWorld3"
    , node:
        node "div"
            Nil
            ( text "Hello World!"
            : Nil
            )
    , expected:
        "<div>Hello World!</div>"
    }


helloWorld4 :: ∀ msg. NodeTest msg
helloWorld4 = NodeTest
    { title: "helloWorld4"
    , node:
        node "div"
            Nil
            ( node "p" Nil (text "Hello World!" : Nil)
            : Nil
            )
    , expected:
        "<div><p>Hello World!</p></div>"
    }


helloWorld5 :: ∀ msg. NodeTest msg
helloWorld5 = NodeTest
    { title: "helloWorld5"
    , node:
        node "div"
            Nil
            ( node "p" Nil (text "Hello to the World!" : Nil)
            : Nil
            )
    , expected:
        "<div><p>Hello to the World!</p></div>"
    }


custom1 :: ∀ msg. NodeTest msg
custom1 =
    NodeTest
        { title: "custom1"
        , node:
            node "div"
                Nil
                ( fromRenderable "String 1"
                : Nil
                )
        , expected:
            "<div>String 1</div>"
        }


custom2 :: ∀ msg. NodeTest msg
custom2 =
    NodeTest
        { title: "custom2"
        , node:
            node "div"
                Nil
                ( fromRenderable "String 2"
                : Nil
                )
        , expected:
            "<div>String 2</div>"
        }


propertyTest :: ∀ msg. NodeTest msg
propertyTest = NodeTest
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


propertyTest2 :: ∀ msg. NodeTest msg
propertyTest2 = NodeTest
    { title: "property2"
    , node:
        node "div"
            ( property "className" (Json.string "meeting")
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div class=\"meeting\">Hello!</div>"
    }


attributeTest :: ∀ msg. NodeTest msg
attributeTest = NodeTest
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


attributeTest2 :: ∀ msg. NodeTest msg
attributeTest2 = NodeTest
    { title: "attribute2"
    , node:
        node "div"
            ( attribute "class" "sleeping"
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div class=\"sleeping\">Hello!</div>"
    }


attributeTest3 :: ∀ msg. NodeTest msg
attributeTest3 = NodeTest
    { title: "attribute3"
    , node:
        node "div"
            ( attribute "href" "http://www.apple.com/"
            : Nil
            )
            ( text "Hello!"
            : Nil
            )
    , expected:
        "<div href=\"http://www.apple.com/\">Hello!</div>"
    }


attributeNsTest :: ∀ msg. NodeTest msg
attributeNsTest = NodeTest
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
styleTest = NodeTest
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
namespacedNode = NodeTest
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
