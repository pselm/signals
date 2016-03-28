module Examples.Graphics.StaticElement where


import Elm.Graphics.Element
import Elm.Text (bold, fromString)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..))
import DOM.Node.Node (appendChild)
import Prelude (bind, Unit, (>>=), (<<<), ($), (<>))
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
        appendChild (elementToNode element) (elementToNode container)


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
        : testColor
        : testTag
        : testImage
        : testFittedImage
        : testTiledImage
        : testCroppedImage
        : testCroppedImage2
        : Nil
        )


title :: String -> Element
title = leftAligned <<< bold <<< fromString


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
    height 28 (title "5. This title has height 28")


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
