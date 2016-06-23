module Examples.Graphics.UpdateElement where


import Elm.Graphics.Element
import Elm.Text (fromString)
import Elm.Color (red, blue, yellow)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Comonad (extract)
import Graphics.Canvas (CANVAS)

import DOM (DOM)
import DOM.Renderable (Position(..), renderIntoDOM, updateDOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlDocumentToEventTarget)
import DOM.HTML.Window (document)
import DOM.HTML.Event.EventTypes (keydown)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.ParentNode (firstElementChild) as ParentNode
import DOM.Node.Types (elementToNode, elementToParentNode, ElementId(..))
import DOM.Event.EventTarget (eventListener, addEventListener)

import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Data.List.Zipper (Zipper(..), up, down, beginning) as Zipper
import Data.Tuple (Tuple(..))

import Prelude (bind, Unit, unit, (>>=), ($), (>>>), pure)


main :: ∀ e. Eff (canvas :: CANVAS, dom :: DOM, ref :: REF | e) Unit
main = do
    doc <-
        window >>= document

    nullableContainer <-
        getElementById (ElementId "contents") (htmlDocumentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        zippedScenes <-
            newRef scenes

        -- Render the first scene
        element <-
            renderIntoDOM ReplacingChildren (elementToNode container) $
                extract (Zipper.beginning scenes)

        let
            move current maybeNext =
                for_ maybeNext \next -> do
                    -- We have to get the first child every time, as it will change!
                    nullableChild <-
                        ParentNode.firstElementChild (elementToParentNode container)

                    for_ (toMaybe nullableChild) \child -> do
                        updateDOM
                            { result: (elementToNode child)
                            , value: extract current
                            }
                            (extract next)

                        writeRef zippedScenes next

            listener =
                eventListener \event -> do
                    current <- readRef zippedScenes

                    case readProp "keyCode" (toForeign event) of
                        -- left key
                        Right 37 ->
                            move current (Zipper.up current)

                        -- right key
                        Right 39 ->
                            move current (Zipper.down current)

                        _ ->
                            pure unit

        addEventListener keydown listener false (htmlDocumentToEventTarget doc)


scenes :: Zipper.Zipper Element
scenes =
    Zipper.Zipper
        Nil
        scene1
        ( scene2 : scene3 : scene4 : scene5 : scene6
        : scene7 : scene8 : scene9 : scene10 : scene11 : scene12
        : scene13 : scene14 : scene15 : scene16 : scene17 : scene18
        : scene19 : scene20 : scene21 : scene22 : scene23 : scene24
        : scene25 : scene26 : scene27 : scene28 : scene29 : scene30
        : scene31 : scene32 : scene33 : scene34 : scene35 : scene36
        : Nil
        )


scene1 :: Element
scene1 =
    title
        """This is scene 1, a simple element to start us off."""


scene2 :: Element
scene2 =
    title
        """Hopefully we have transitioned to scene 2. How did that go?"""


scene3 :: Element
scene3 =
    title
        """This is scene 3 and scene 4. So, when you go forward the first time,
        you shouldn't notice any change. The second time, you should transition
        to scene 5."""


scene4 :: Element
scene4 = scene3


scene5 :: Element
scene5 =
    link "http://apple.com" $
        title
            """As promised, here is scene5. The whole thing should be a link to
            http://apple.com/"""


scene6 :: Element
scene6 =
    title """Now, for scene 6, we'll remove the link ... how did that go?"""


scene7 :: Element
scene7 =
    title """I hope removing the link worked OK."""


blueBox :: Element
blueBox =
    color blue (spacer 20 20)


redBox :: Element
redBox =
    color red (spacer 30 40)


title :: String -> Element
title = fromString >>> leftAligned


scene8 :: Element
scene8 =
    flow down
        ( title "Here's a little blue box. I'll change its properties and we'll see what happens."
        : blueBox
        : Nil
        )

scene9 :: Element
scene9 =
    flow down
        ( title "Ha! I didn't actually change any properties this time. Fooled you!"
        : blueBox
        : Nil
        )


scene10 :: Element
scene10 =
    flow down
        ( title "This time I made it wider."
        : width 50 blueBox
        : Nil
        )


scene11 :: Element
scene11 =
    flow down
        ( title "Now I made it taller."
        : height 50 blueBox
        : Nil
        )


scene12 :: Element
scene12 =
    flow down
        ( title "Now, reduced opacity."
        : opacity 0.5 blueBox
        : Nil
        )


scene13 :: Element
scene13 =
    flow down
        ( title "And different color."
        : color red blueBox
        : Nil
        )


scene14 :: Element
scene14 =
    flow down
        ( title "Give it an ID"
        : tag "blueBox" blueBox
        : Nil
        )


scene15 :: Element
scene15 =
    flow down
        ( title "Make it a link to apple.com"
        : link "http://apple.com" blueBox
        : Nil
        )


scene16 :: Element
scene16 =
    flow down
        ( title "Change the link to purescript.org"
        : link "http://purescript.org" blueBox
        : Nil
        )


scene17 :: Element
scene17 =
    flow down
        ( title "And just a blue box again"
        : blueBox
        : Nil
        )


scene18 :: Element
scene18 =
    flow down
        ( title "So, now we'll test some element transitions. First, spacer -> spacer ... here's a red box"
        : redBox
        : Nil
        )

scene19 :: Element
scene19 =
    flow down
        ( title "Now, spacer to text ... here's some text"
        : leftAligned (fromString "Some text.")
        : Nil
        )


scene20 :: Element
scene20 =
    flow down
        ( title "So, we'll just change the text."
        : leftAligned (fromString "Changed text.")
        : Nil
        )


scene21 :: Element
scene21 =
    flow down
        ( title "This time, we'll change a prop, to color blue"
        : color blue (leftAligned (fromString "Changed text."))
        : Nil
        )


scene22 :: Element
scene22 =
    flow down
        ( title "Now, let's show a picture of a grotesque head."
        : image 200 150 "head.png"
        : Nil
        )


scene23 :: Element
scene23 =
    flow down
        ( title "Change the src='' to a library."
        : image 200 150 "library.png"
        : Nil
        )


scene24 :: Element
scene24 =
    flow down
        ( title "Change the image size."
        : image 400 200 "library.png"
        : Nil
        )

scene25 :: Element
scene25 =
    flow down
        ( title "Change to a fitted image"
        : fittedImage 400 200 "library.png"
        : Nil
        )


scene26 :: Element
scene26 =
    flow down
        ( title "Change to a cropped image"
        : croppedImage (Tuple 100 50) 75 50 "library.png"
        : Nil
        )


scene27 :: Element
scene27 =
    flow down
        ( title "Change the cropping"
        : croppedImage (Tuple 50 100) 75 50 "library.png"
        : Nil
        )


scene28 :: Element
scene28 =
    flow down
        ( title "Change to tiled image"
        : tiledImage 400 200 "library.png"
        : Nil
        )


smallBlueBox :: Element
smallBlueBox = color blue (spacer 20 20)


smallRedBox :: Element
smallRedBox = color red (spacer 20 20)


smallYellowBox :: Element
smallYellowBox = color yellow (spacer 20 20)


scene29 :: Element
scene29 =
    flow down
        ( title "Two boxes flowing down"
        : flow down (smallRedBox : smallBlueBox : Nil)
        : Nil
        )


scene30 :: Element
scene30 =
    flow down
        ( title "Add a third box"
        : flow down (smallRedBox : smallBlueBox : smallYellowBox : Nil)
        : Nil
        )


scene31 :: Element
scene31 =
    flow down
        ( title "Keep direction, but rotate elements"
        : flow down (smallYellowBox : smallRedBox : smallBlueBox : Nil)
        : Nil
        )


scene32 :: Element
scene32 =
    flow down
        ( title "Change one element"
        : flow down (smallYellowBox : smallYellowBox : smallBlueBox : Nil)
        : Nil
        )


scene33 :: Element
scene33 =
    flow down
        ( title "Reverse the direction"
        : flow up (smallYellowBox : smallYellowBox : smallBlueBox : Nil)
        : Nil
        )


redColor :: Element -> Element
redColor = color red


scene34 :: Element
scene34 =
    flow down
        ( title "Container"
        : redColor (container 100 100 middle smallBlueBox)
        : Nil
        )


scene35 :: Element
scene35 =
    flow down
        ( title "Container with changed position"
        : redColor (container 100 100 topLeft smallBlueBox)
        : Nil
        )


scene36 :: Element
scene36 =
    flow down
        ( title "Container with changed size"
        : redColor (container 150 150 topLeft smallBlueBox)
        : Nil
        )
