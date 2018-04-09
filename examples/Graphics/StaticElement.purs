module Examples.Graphics.StaticElement
    ( elements
    , Expectation, expectations
    , main
    ) where

import Elm.Graphics.Element
import Elm.Text (bold, fromString)
import Elm.Color (blue, red, yellow, lightBlue)

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS)

import DOM (DOM)
import DOM.Renderable (render)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)

import Data.Foldable (for_)
import Data.List (List(..), (:), fromFoldable)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Prelude (bind, Unit, (>>=), ($), (<>), (<$>), map)
import Prelude (show) as Prelude


main :: ∀ e. Eff (canvas :: CANVAS, dom :: DOM | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ nullableContainer \container -> do
        element <- render (htmlDocumentToDocument doc) scene
        appendChild element (elementToNode container)


scene :: Element
scene =
    flow down (fromFoldable elements)


elements :: NonEmpty Array Element
elements =
    map makeExample expectations


makeExample :: Expectation -> Element
makeExample expectation =
    flow down
        ( title expectation.title
        : fromFoldable expectation.elements
        )


type Expectation =
    { title :: String
    , elements :: Array Element
    , expected :: Maybe String
    }


expectations :: NonEmpty Array Expectation
expectations =
    NonEmpty helloWorld $
        [ testLink
        , testEmpty
        , testWidthOfHeightOfSizeOf
        , testHeight
        , testSize
        , testWidth
        , testOpacity
        , testTag
        , testColor
        , testImage
        , testFittedImage
        , testTiledImage
        , testCroppedImage
        , testCroppedImage2
        , testTextAlignment
        , testShow
        , testContainer
        , testAbove
        , testBelow
        , testBeside
        , testLayers
        , testPositions
        , testBigPositions
        ] <> testFlowDirections


title :: String -> Element
title text =
    flow down
        ( spacer 10 10
        : leftAligned (bold (fromString text))
        : Nil
        )


helloWorld :: Expectation
helloWorld =
    { title: "1. Hello World"
    , elements: []
    , expected: Just "<div style=\"padding: 0px; margin: 0px; width: 0px; height: 0px;\"></div>"
    }


testLink :: Expectation
testLink =
    { title: "2. This element links to '#number9'"
    , elements: [link "#number9" $ leftAligned $ fromString "A link here"]
    , expected: Just $
        "<div style=\"padding: 0px; margin: 0px; width: 0px; height: 0px;\">" <>
            "<a style=\"padding: 0px; margin: 0px; display: block; pointer-events: auto;\" href=\"#number9\">" <>
                "<div style=\"padding: 0px; margin: 0px; visibility: visible; text-align: left; pointer-events: auto; width: 0px; height: 0px;\">A link here</div>" <>
            "</a>" <>
        "</div>"
    }


testEmpty :: Expectation
testEmpty =
    { title: "3. There is an empty element under this"
    , elements: [empty]
    , expected: Nothing
    }


testWidthOfHeightOfSizeOf :: Expectation
testWidthOfHeightOfSizeOf =
    { title: "4. The width, height and size of the next element"
    , elements: [widths]
    , expected: Nothing
    }

    where
        widths =
            leftAligned $ fromString $
                "widthOf: " <> Prelude.show (widthOf test)
                <> "\nheightOf: " <> Prelude.show (heightOf test)
                <> "\nsizeOf: {width: " <> Prelude.show s.width
                <> ", height: " <> Prelude.show s.height <> "}"

        s =
            sizeOf test

        test =
            height 40 $ leftAligned $ fromString "This should be big"


testHeight :: Expectation
testHeight =
    { title: "5. This element has height 40"
    , elements: [height 40 $ leftAligned $ fromString "This should be big"]
    , expected: Nothing
    }


testSize :: Expectation
testSize =
    { title: "6. Set size of centred text to width: 200, height: 32"
    , elements: [size 200 32 (centered $ fromString $ "Centered")]
    , expected: Nothing
    }


testWidth :: Expectation
testWidth =
    { title: "7. Set width of centred text to 400"
    , elements: [ width 400 (centered $ fromString $ "Centered") ]
    , expected: Nothing
    }


testOpacity :: Expectation
testOpacity =
    { title: "8. This element has 50% opacity"
    , elements: [ opacity 0.5 $ leftAligned $ fromString "This should be lighter" ]
    , expected: Nothing
    }


testTag :: Expectation
testTag =
    { title: "9. This element has ID 'number9'"
    , elements: [ tag "number9" $ leftAligned $ fromString "Here is the element with the ID" ]
    , expected: Nothing
    }


testColor :: Expectation
testColor =
    { title: "10. This element has a blue background"
    , elements: [ color lightBlue $ leftAligned $ fromString "The element with a blue background" ]
    , expected: Nothing
    }


testImage :: Expectation
testImage =
    { title: "11. A plain image, with width 200 height 150"
    , elements: [ image 200 150 "head.png" ]
    , expected: Nothing
    }


testFittedImage :: Expectation
testFittedImage =
    { title: "12. A fitted impage, with width 200 height 150"
    , elements: [ fittedImage 200 150 "head.png" ]
    , expected: Nothing
    }


testTiledImage :: Expectation
testTiledImage =
    { title: "13. A tiled image, with width 600 height 200"
    , elements: [ tiledImage 600 200 "head.png" ]
    , expected: Nothing
    }


testCroppedImage :: Expectation
testCroppedImage =
    { title: "14. A cropped image, starting at top 20, left 10 and using width 40, height 60"
    , elements: [ croppedImage (Tuple 20 10) 40 60 "head.png" ]
    , expected: Nothing
    }


testCroppedImage2 :: Expectation
testCroppedImage2 =
    { title: "15. Like 14, but height subsequently increased to 120"
    , elements: [ height 120 (croppedImage (Tuple 20 10) 40 60 "head.png") ]
    , expected: Nothing
    }


testTextAlignment :: Expectation
testTextAlignment =
    { title: "16. Text alignment in a 200 width element"
    , elements:
        width 200 <$>
            [ leftAligned (fromString ("leftAligned"))
            , centered (fromString ("centered"))
            , rightAligned (fromString ("rightAligned"))
            , justified (fromString ("This text is justified, so I'll have to make it longer."))
            ]
    , expected: Nothing
    }


testShow :: Expectation
testShow =
    { title: "17. Show"
    , elements: [ show "Some string to show" ]
    , expected: Nothing
    }


smallBlueBox :: Element
smallBlueBox = color blue (spacer 20 20)


smallRedBox :: Element
smallRedBox = color red (spacer 20 20)


bigBlueBox :: Element
bigBlueBox = color blue (spacer 40 40)


smallYellowBox :: Element
smallYellowBox = color yellow (spacer 20 20)


redColor :: Element -> Element
redColor = color red


testContainer :: Expectation
testContainer =
    { title: "18. redColor (container 100 100 middle smallBlueBox)"
    , elements: [ redColor (container 100 100 middle smallBlueBox) ]
    , expected: Nothing
    }


testAbove :: Expectation
testAbove =
    { title: "19. smallBlueBox `above` smallRedBox"
    , elements: [ smallBlueBox `above` smallRedBox ]
    , expected: Nothing
    }


testBelow :: Expectation
testBelow =
    { title: "20. smallBlueBox `below` smallRedBox"
    , elements: [smallBlueBox `below` smallRedBox]
    , expected: Nothing
    }


testBeside :: Expectation
testBeside =
    { title: "21. smallBlueBox `beside` smallRedBox"
    , elements: [ smallBlueBox `beside` smallRedBox ]
    , expected: Nothing
    }


testLayers :: Expectation
testLayers =
    { title: "22. layers (bigBlueBox : smallRedBox : Nil)"
    , elements: [ layers (bigBlueBox : smallRedBox : Nil) ]
    , expected: Nothing
    }


testPosition :: Position -> String -> Element
testPosition position text =
    container 120 120 middle $
        redColor $
            container 100 100 position $
                color lightBlue $
                    leftAligned $ fromString $ text


testPositions :: Expectation
testPositions =
    { title: "23. Various positions"
    , elements:
        [ flow right
            ( testPosition middle "middle"
            : testPosition topLeft "topLeft"
            : testPosition topRight "topRight"
            : testPosition bottomLeft "bottomLeft"
            : testPosition bottomRight "bottomRight"
            : testPosition midLeft "midLeft"
            : testPosition midRight "midRight"
            : testPosition midTop "midTop"
            : testPosition midBottom "midBottom"
            : Nil
            )
        ]
    , expected: Nothing
    }


testBigPosition :: Position -> String -> Element
testBigPosition position text =
    container 220 220 middle $
        redColor $
            container 200 200 position $
                color lightBlue $
                    leftAligned $ fromString $ text


testBigPositions :: Expectation
testBigPositions =
    { title: "24. Various other positions"
    , elements:
        [ flow right
            ( testBigPosition (middleAt (absolute 25) (absolute 25)) "middleAt (a 25) (a 25)"
            : testBigPosition (middleAt (relative 0.25) (relative 0.25)) "middleAt (r 0.25) (r 0.25)"
            : testBigPosition (topLeftAt (absolute 25) (absolute 25)) "topLeftAt (a 25) (a 25)"
            : testBigPosition (topLeftAt (relative 0.25) (relative 0.25)) "topLeftAt (r 0.25) (r 0.25)"
            : testBigPosition (topRightAt (absolute 25) (absolute 25)) "topRightAt (a 25) (a 25)"
            : testBigPosition (topRightAt (relative 0.25) (relative 0.25)) "topRightAt (r 0.25) (r 0.25)"
            : testBigPosition (bottomLeftAt (absolute 25) (absolute 25)) "bottomLeftAt (a 25) (a 25)"
            : testBigPosition (bottomLeftAt (relative 0.25) (relative 0.25)) "bottomLeftAt (r 0.25) (r 0.25)"
            : testBigPosition (bottomRightAt (absolute 25) (absolute 25)) "bottomRightAt (a 25) (a 25)"
            : testBigPosition (bottomRightAt (relative 0.25) (relative 0.25)) "bottomRightAt (r 0.25) (r 0.25)"
            : testBigPosition (midLeftAt (absolute 25) (absolute 25)) "midLeftAt (a 25) (a 25)"
            : testBigPosition (midLeftAt (relative 0.25) (relative 0.25)) "midLeftAt (r 0.25) (r 0.25)"
            : testBigPosition (midRightAt (absolute 25) (absolute 25)) "midRightAt (a 25) (a 25)"
            : testBigPosition (midRightAt (relative 0.25) (relative 0.25)) "midRightAt (r 0.25) (r 0.25)"
            : testBigPosition (midTopAt (absolute 25) (absolute 25)) "midTopAt (a 25) (a 25)"
            : testBigPosition (midTopAt (relative 0.25) (relative 0.25)) "midTopAt (r 0.25) (r 0.25)"
            : testBigPosition (midBottomAt (absolute 25) (absolute 25)) "midBottomAt (a 25) (a 25)"
            : testBigPosition (midBottomAt (relative 0.25) (relative 0.25)) "midBottomAt (r 0.25) (r 0.25)"
            : Nil
            )
        ]
    , expected: Nothing
    }


-- The background-color doesn't show up in all of these -- it seems to be a JSDOM problem.
testFlowDirections :: Array Expectation
testFlowDirections =
    [ { title: "25. flow down (red, blue, yellow)"
      , elements: [ flow down (smallRedBox : smallBlueBox : smallYellowBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 60px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 60px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    , { title: "26. flow up (red, blue, yellow)"
      , elements: [ flow up (smallRedBox : smallBlueBox : smallYellowBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 60px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 60px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    , { title: "27. flow left (red, blue, yellow)"
      , elements: [ flow left (smallRedBox : smallBlueBox : smallYellowBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 60px; height: 20px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; width: 60px; height: 20px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    , { title: "28. flow right (red, blue, yellow)"
      , elements: [ flow right (smallRedBox : smallBlueBox : smallYellowBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 60px; height: 20px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; width: 60px; height: 20px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; float: left;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    , { title: "29. flow outward (big blue, red)"
      , elements: [ flow outward (bigBlueBox : smallRedBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 40px; height: 40px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; pointer-events: none; width: 40px; height: 40px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 40px; height: 40px; position: absolute;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; position: absolute;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    , { title: "30. flow inward (red, big blue) -- so, the same as above"
      , elements: [ flow inward (smallRedBox : bigBlueBox : Nil) ]
      , expected: Just $
              "<div style=\"padding: 0px; margin: 0px; width: 40px; height: 40px;\">" <>
                  "<div style=\"padding: 0px; margin: 0px; pointer-events: none; width: 40px; height: 40px;\">" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 40px; height: 40px; position: absolute;\"></div>" <>
                      "<div style=\"padding: 0px; margin: 0px; width: 20px; height: 20px; position: absolute;\"></div>" <>
                  "</div>" <>
              "</div>"
      }
    ]
