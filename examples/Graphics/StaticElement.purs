module Examples.Graphics.StaticElement where


import Elm.Graphics.Element
import Elm.Text (bold, fromString)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Renderable (render)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)
import Prelude (bind, Unit, (>>=), ($), (<>), (<$>))
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))


main :: ∀ e. Eff (dom :: DOM | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        element <- render scene
        appendChild element (elementToNode container)


scene :: Element
scene =
    flow down
        ( helloWorld
        : testLink
        : testEmpty
        : testWidthOfHeightOfSizeOf
        : testHeight
        : testSize
        : testWidth
        : testOpacity
        : testTag
        : testColor
        : testImage
        : testFittedImage
        : testTiledImage
        : testCroppedImage
        : testCroppedImage2
        : testTextAlignment
        : testShow
        : testContainer
        : testAboveBelowBeside
        : testLayers
        : testPositions
        : testBigPositions
        : testFlowDirections
        : Nil
        )


title :: String -> Element
title text =
    flow down
        ( spacer 10 10
        : leftAligned (bold (fromString text))
        : Nil
        )


helloWorld :: Element
helloWorld = title "1. Hello World!"


testLink :: Element
testLink = link "#number9" $ title "2. This title is a linke to '#number9'"


testEmpty :: Element
testEmpty =
    flow down
        ( title "3. There is an empty element under this"
        : empty
        : Nil
        )

testWidthOfHeightOfSizeOf :: Element
testWidthOfHeightOfSizeOf =
    flow down
        ( title "4. The width, height and size of the previous element"
        : widths
        : Nil
        )

    where
        widths =
            leftAligned $ fromString $
                "widthOf: " <> Prelude.show (widthOf testEmpty)
                <> "\nheightOf: " <> Prelude.show (heightOf testEmpty)
                <> "\nsizeOf: {width: " <> Prelude.show s.width
                <> ", height: " <> Prelude.show s.height <> "}"

        s =
            sizeOf testEmpty


testHeight :: Element
testHeight =
    height 40 (title "5. This title has height 40")


testSize :: Element
testSize =
    flow down
        ( title "6. Set size of centred text to width: 200, height: 32"
        : size 200 32 (centered $ fromString $ "Centered")
        : Nil
        )


testWidth :: Element
testWidth =
    flow down
        ( title "7. Set width of centred text to 400"
        : width 400 (centered $ fromString $ "Centered")
        : Nil
        )


testOpacity :: Element
testOpacity = opacity 0.5 $ title "8. This title has 50% opacity"


testTag :: Element
testTag = tag "number9" $ title "9. This title has ID 'number9'"


testColor :: Element
testColor = color Elm.Color.lightBlue $ title "10. This title has a blue background"


testImage :: Element
testImage =
    flow down
        ( title "11. A plain image, with width 200 height 150"
        : image 200 150 "head.png"
        : Nil
        )

testFittedImage :: Element
testFittedImage =
    flow down
        ( title "12. A fitted impage, with width 200 height 150"
        : fittedImage 200 150 "head.png"
        : Nil
        )


testTiledImage :: Element
testTiledImage =
    flow down
        ( title "13. A tiled image, with width 600 height 200"
        : tiledImage 600 200 "head.png"
        : Nil
        )


testCroppedImage :: Element
testCroppedImage =
    flow down
        ( title "14. A cropped image, starting at top 20, left 10 and using width 40, height 60"
        : croppedImage {top: 20, left: 10} 40 60 "head.png"
        : Nil
        )


testCroppedImage2 :: Element
testCroppedImage2 =
    flow down
        ( title "15. Like 14, but height subsequently increased to 120"
        : height 120 (croppedImage {top: 20, left: 10} 40 60 "head.png")
        : Nil
        )


testTextAlignment :: Element
testTextAlignment =
    flow down $
        width 200 <$>
            ( title "16. Text alignment in a 200 width element"
            : leftAligned (fromString ("leftAligned"))
            : centered (fromString ("centered"))
            : rightAligned (fromString ("rightAligned"))
            : justified (fromString ("This text is justified, so I'll have to make it longer."))
            : Nil
            )


testShow :: Element
testShow =
    flow down
        ( title "17. Show"
        : show "Some string to show"
        : Nil
        )


smallBlueBox :: Element
smallBlueBox = color Elm.Color.blue (spacer 20 20)


smallRedBox :: Element
smallRedBox = color Elm.Color.red (spacer 20 20)


bigBlueBox :: Element
bigBlueBox = color Elm.Color.blue (spacer 40 40)


smallYellowBox :: Element
smallYellowBox = color Elm.Color.yellow (spacer 20 20)


redColor :: Element -> Element
redColor = color Elm.Color.red


testContainer :: Element
testContainer =
    flow down
        ( title "18. redColor (container 100 100 middle smallBlueBox)"
        : redColor (container 100 100 middle smallBlueBox)
        : Nil
        )

testAboveBelowBeside :: Element
testAboveBelowBeside =
    flow down
        ( title "19. smallBlueBox `above` smallRedBox"
        : smallBlueBox `above` smallRedBox
        : title "20. smallBlueBox `below` smallRedBox"
        : smallBlueBox `below` smallRedBox
        : title "21. smallBlueBox `beside` smallRedBox"
        : smallBlueBox `beside` smallRedBox
        : Nil
        )


testLayers :: Element
testLayers =
    flow down
        ( title "22. layers (bigBlueBox : smallRedBox : Nil)"
        : layers (bigBlueBox : smallRedBox : Nil)
        : Nil
        )


testPosition :: Position -> String -> Element
testPosition position text =
    container 120 120 middle $
        redColor $
            container 100 100 position $
                color Elm.Color.lightBlue $
                    leftAligned $ fromString $ text


testPositions :: Element
testPositions =
    flow down 
        ( title "23. Various positions"
        : flow right
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
        : Nil
        )


testBigPosition :: Position -> String -> Element
testBigPosition position text =
    container 220 220 middle $
        redColor $
            container 200 200 position $
                color Elm.Color.lightBlue $
                    leftAligned $ fromString $ text


testBigPositions :: Element
testBigPositions =
    flow down 
        ( title "24. Various other positions"
        : flow right
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
        : Nil
        )


testFlowDirections :: Element
testFlowDirections =
    flow down
        ( title "25. flow down (red, blue, yellow)"
        : flow down (smallRedBox : smallBlueBox : smallYellowBox : Nil)
        : title "26. flow up (red, blue, yellow)"
        : flow up (smallRedBox : smallBlueBox : smallYellowBox : Nil)
        : title "27. flow left (red, blue, yellow)"
        : flow left (smallRedBox : smallBlueBox : smallYellowBox : Nil)
        : title "28. flow right (red, blue, yellow)"
        : flow right (smallRedBox : smallBlueBox : smallYellowBox : Nil)
        : title "29. flow outward (big blue, red)"
        : flow outward (bigBlueBox : smallRedBox : Nil)
        : title "30. flow inward (red, big blue) -- so, the same as above"
        : flow inward (smallRedBox : bigBlueBox : Nil)
        : Nil
        )
