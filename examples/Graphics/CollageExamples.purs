
module Examples.Graphics.CollageExamples
    ( Example, examples
    ) where


import Elm.Graphics.Collage
import Elm.Graphics.Element (Element, flow, down, spacer, color)
import Elm.Graphics.Internal (setStyle)
import Elm.Color (red, linear, radial, rgb, rgba, hsl, white, blue, purple, green, yellow)
import Elm.Basics ((|>), degrees)
import Elm.Text (Line(..), typeface, join, line, fromString, height, color, bold, italic, monospace) as Text

import Control.Monad (when)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (elements)
import Math (sin, cos)
import Data.List (List(..), (..), (:), fromList)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)

import DOM.Renderable (class Renderable, render)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Types (elementToNode, textToNode)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)

import Prelude (bind, pure, unit, (>>=), ($), (<$>), negate, (/=), (#), (*))


newtype Example = Example
    { caption :: String
    , collage :: Collage
    , reference :: String
    }

instance renderableExample :: Renderable Example where
    render (Example example) = do
        -- I wonder whether `render` ought to supply the document? But I guess it's
        -- going to be slightly inconvenient either way.
        doc <-
            htmlDocumentToDocument <$>
                (window >>= document)

        row <- elementToNode <$> createElement "tr" doc

        column1 <- createElement "td" doc
        column2 <- createElement "td" doc
        column3 <- createElement "td" doc
        column4 <- createElement "td" doc

        setStyle "border" "1px solid blue" column1
        setStyle "border" "1px solid blue" column2
        setStyle "border" "1px solid blue" column3
        setStyle "border" "1px solid blue" column4

        appendChild (elementToNode column1) row
        appendChild (elementToNode column2) row
        appendChild (elementToNode column3) row
        appendChild (elementToNode column4) row

        caption <- elementToNode <$> createElement "pre" doc
        text <- textToNode <$> createTextNode example.caption doc
        appendChild text caption
        appendChild caption (elementToNode column1)

        collage <- render example.collage
        appendChild collage (elementToNode column2)

        when (example.reference /= "") do
            image <- createElement "img" doc
            setAttribute "src" example.reference image
            appendChild (elementToNode image) (elementToNode column3)

            setStyle "background-color" "black" column4

            mixed <- createElement "div" doc
            setStyle "position" "relative" mixed
            setStyle "isolation" "isolate" mixed
            appendChild (elementToNode mixed) (elementToNode column4)

            -- This one is just to take up space ...
            image2 <- createElement "img" doc
            setAttribute "src" example.reference image2
            setAttribute "visibility" "hidden" image2
            appendChild (elementToNode image2) (elementToNode column4)

            image3 <- createElement "img" doc
            setAttribute "src" example.reference image3
            setStyle "position" "absolute" image3
            setStyle "left" "0px" image3
            setStyle "top" "0px" image3
            appendChild (elementToNode image3) (elementToNode mixed)

            collage2 <- render example.collage
            div <- createElement "div" doc
            setStyle "position" "absolute" div
            setStyle "left" "0px" div
            setStyle "top" "0px" div
            setStyle "mix-blend-mode" "difference" div
            appendChild collage2 (elementToNode div)
            appendChild (elementToNode div) (elementToNode mixed)

            pure unit

        pure row

    -- Note that I really want
    --
    --     update = defaultUpdate
    --
    -- ... but the compiler complains.
    update rendered = render


instance arbitraryExample :: Arbitrary Example where
    arbitrary =
        case examples of
            Cons head tail ->
                elements head (fromList tail)

            Nil ->
                pure example1


examples :: List Example
examples =
    ( example1 : example2 : example3 : example4 : example5 : example6
    : example7 : example8 : example9 : example10
    : example15 : example16 : example17 : example18
    : example19 : example20 : example21 : example22 : example23 : example24
    : example25 : example26 : example27 : example28 : example29
    : Nil
    )


example1 :: Example
example1 =
    Example
        { caption:
            """
            makeCollage 50 50
                ( filled red (rect 10.0 10.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example1.png"
        , collage:
            makeCollage 50 50
                ( filled red (rect 10.0 10.0)
                : Nil
                )
        }


example2 :: Example
example2 =
    Example
        { caption:
            """
            makeCollage 50 50
                ( filled red (rect 20.0 20.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example2.png"
        , collage:
            makeCollage 50 50
                ( filled red (rect 20.0 20.0)
                : Nil
                )
        }


example3 :: Example
example3 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( filled red (rect 20.0 20.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example3.png"
        , collage:
            makeCollage 100 100
                ( filled red (rect 20.0 20.0)
                : Nil
                )
        }


example4 :: Example
example4 =
    Example
        { caption:
            """
            makeCollage 50 50
                ( filled red (rect 50.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example4.png"
        , collage:
            makeCollage 50 50
                ( filled red (rect 50.0 50.0)
                : Nil
                )
        }


example5 :: Example
example5 =
    Example
        { caption:
            """
            makeCollage 50 50
                ( filled red (rect 100.0 100.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example5.png"
        , collage:
            makeCollage 50 50
                ( filled red (rect 100.0 100.0)
                : Nil
                )
        }


-- TODO: This one doesn't seem to work
example6 :: Example
example6 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( textured "texture.jpg" (rect 100.0 100.0)
                : Nil
                )
            """
        , reference: ""
        , collage:
            makeCollage 100 100
                ( textured "texture.jpg" (rect 100.0 100.0)
                : Nil
                )
        }


-- TODO: This one is kind of upside-down compared to what it should be ...
-- compare http://elm-lang.org/examples/linear-gradient
example7 :: Example
example7 =
    Example
        { caption: "Linear gradient"
        , reference: "StaticCollage/example7.png"
        , collage:
            makeCollage 120 120
                ( gradient grad (rect 120.0 120.0)
                : Nil
                )
        }

    where
        grad =
            linear
                (Tuple 0.0 60.0)
                (Tuple 0.0 (-60.0))
                ( Tuple 0.0 (rgb 0 171 235)
                : Tuple 0.79 white
                : Tuple 0.8 (rgb 38 192 0)
                : Tuple 1.0 white
                : Nil
                )


-- TODO: This one is also close, but not quite right ...
-- compare http://elm-lang.org/examples/radial-gradient
example8 :: Example
example8 =
    Example
        { caption: "Radial gradient"
        , reference: "StaticCollage/example8.png"
        , collage:
            makeCollage 300 300
                ( move (Tuple (-55.0) (-55.0)) (gradient grad1 (circle 100.0))
                : move (Tuple   40.0    85.0)  (gradient grad2 (circle 100.0))
                : move (Tuple   50.0  (-10.0)) (gradient grad3 (circle 100.0))
                : move (Tuple (-10.0)   50.0)  (gradient grad4 (circle 100.0))
                : Nil
                )
        }

    where
        grad1 =
            radial
                (Tuple 0.0  0.0) 50.0
                (Tuple 0.0 10.0) 90.0
                ( Tuple 0.0 (rgb  244 242 1)
                : Tuple 0.8 (rgb  228 199 0)
                : Tuple 1.0 (rgba 228 199 0 0.0)
                : Nil
                )

        grad2 =
            radial
                (Tuple 0.0  0.0)   15.0
                (Tuple 7.0 (-5.0)) 40.0
                ( Tuple 0.0 (rgb  0 201 255)
                : Tuple 0.8 (rgb  0 181 226)
                : Tuple 1.0 (rgba 0 181 226 0.0)
                : Nil
                )

        grad3 =
            radial
                (Tuple 0.0    0.0)  20.0
                (Tuple 7.0 (-15.0)) 50.0
                ( Tuple  0.0 (rgb  255 95 152)
                : Tuple 0.75 (rgb  255 1 136)
                : Tuple  1.0 (rgba 255 1 136 0.0)
                : Nil
                )

        grad4 =
            radial
                (Tuple 0.0   0.0)  10.0
                (Tuple 7.0 (-5.0)) 30.0
                ( Tuple 0.0 (rgb  167 211 12)
                : Tuple 0.9 (rgb  1 159 98)
                : Tuple 1.0 (rgba 1 159 98 0.0)
                : Nil
                )


hook :: Path
hook =
    path
        ( Tuple (-100.0)   20.0
        : Tuple ( -70.0)    0.0
        : Tuple (-100.0) (-20.0)
        : Nil
        )


example9 :: Example
example9 =
    Example
        { caption:
            """
            makeCollage 200 100
                ( moveX 0.0 (traced defaultLine hook)
                : moveX 10.0 (traced defaultLine { color = red } hook)
                : moveX 20.0 (traced defaultLine { width = 8.0, join = Smooth } hook)
                : moveX 40.0 (traced defaultLine { width = 8.0, join = Clipped } hook)
                : moveX 60.0 (traced defaultLine { width = 8.0, join = Sharp 5.0 } hook)
                : moveX 80.0 (traced defaultLine { width = 8.0, join = Sharp 10.0 } hook)
                : moveX 100.0 (traced defaultLine { width = 8.0, cap = Flat, color = blue } hook)
                : moveX 120.0 (traced defaultLine { width = 8.0, cap = Padded, color = blue } hook)
                : moveX 140.0 (traced defaultLine { width = 8.0, cap = Round, color = blue } hook)
                )
            """
        , reference: "StaticCollage/example9.png"
        , collage:
            makeCollage 200 100
                ( moveX 0.0 (traced defaultLine hook)
                : moveX 10.0 (traced defaultLine { color = red } hook)
                : moveX 20.0 (traced defaultLine { width = 8.0, join = Smooth } hook)
                : moveX 40.0 (traced defaultLine { width = 8.0, join = Clipped } hook)
                : moveX 60.0 (traced defaultLine { width = 8.0, join = Sharp 5.0 } hook)
                : moveX 80.0 (traced defaultLine { width = 8.0, join = Sharp 10.0 } hook)
                : moveX 100.0 (traced defaultLine { width = 8.0, cap = Flat, color = blue } hook)
                : moveX 120.0 (traced defaultLine { width = 8.0, cap = Padded, color = blue } hook)
                : moveX 140.0 (traced defaultLine { width = 8.0, cap = Round, color = blue } hook)
                : Nil
                )
        }


smallBlueBox :: Element
smallBlueBox = color blue (spacer 20 20)


smallRedBox :: Element
smallRedBox = color red (spacer 20 20)


smallYellowBox :: Element
smallYellowBox = color yellow (spacer 20 20)


boxes :: Element
boxes =
    flow down (smallRedBox : smallBlueBox : smallYellowBox : Nil)


example10 :: Example
example10 =
    Example
        { caption:
            """
            makeCollage 300 300
                ( toForm boxes
                , ( toForm boxes
                    |> rotate (degrees 45)
                    |> moveX -40.0
                    |> moveY -40.0
                    |> scale 1.5
                  )
                : Nil
                )
            """
        , reference: "StaticCollage/example10.png"
        , collage:
            makeCollage 300 300
                ( toForm boxes
                : ( toForm boxes
                    |> rotate (degrees 45.0)
                    |> moveX (-40.0)
                    |> moveY (-40.0)
                    |> scale 1.5
                    |> alpha 0.5
                  )
                : Nil
                )
        }


example15 :: Example
example15 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (solid red) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example15.png"
        , collage:
            makeCollage 100 100
                ( outlined (solid red) (rect 100.0 50.0)
                : Nil
                )
        }


example16 :: Example
example16 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (dashed red) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example16.png"
        , collage:
            makeCollage 100 100
                ( outlined (dashed red) (rect 100.0 50.0)
                : Nil
                )
        }


example17 :: Example
example17 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (dotted red) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example17.png"
        , collage:
            makeCollage 100 100
                ( outlined (dotted red) (rect 100.0 50.0)
                : Nil
                )
        }


example18 :: Example
example18 =
    Example
        { caption: "http://elm-lang.org/examples/lines"
        , reference: "StaticCollage/example18.png"
        , collage:
            makeCollage 200 420
                ( move (Tuple 0.0 (-55.0)) blueSquare
                : move (Tuple 0.0  (55.0)) redSquare
                : Nil
                )
        }

    where
        blueSquare =
            traced (dashed blue) square

        redSquare =
            traced (solid red) square

        square =
            path
                ( (Tuple   50.0    50.0)
                : (Tuple   50.0  (-50.0))
                : (Tuple (-50.0) (-50.0))
                : (Tuple (-50.0)   50.0)
                : (Tuple   50.0    50.0)
                : Nil
                )


example19 :: Example
example19 =
    Example
        { caption: "http://elm-lang.org/examples/shapes"
        , reference: "StaticCollage/example19.png"
        , collage:
            makeCollage 300 300
                ( ( ngon 4 75.0
                    # filled clearGrey
                    # move (Tuple (-10.0) 0.0)
                  )
                : ( ngon 5 50.0
                    # filled clearGrey
                    # move (Tuple 50.0 10.0)
                  )
                : Nil
                )
        }

    where
        clearGrey =
            rgba 111 111 111 0.6


example20 :: Example
example20 =
    Example
        { caption: "http://elm-lang.org/examples/transforms"
        , reference: "StaticCollage/example20.png"
        , collage:
            makeCollage 300 300
                ( ( hexagon red )
                : ( hexagon purple
                    |> scale 2.0
                  )
                : ( hexagon green
                    |> move (Tuple 100.0 0.0)
                  )
                : ( hexagon blue
                    |> rotate (degrees 30.0)
                  )
                : Nil
                )
        }

    where
        hexagon clr =
            outlined (solid clr) (ngon 6 40.0)


example21 :: Example
example21 =
    Example
        { caption: "http://elm-lang.org/examples/color"
        , reference: "StaticCollage/example21.png"
        , collage:
            makeCollage 150 150 (shape <$> (0..11))
        }

    where
        shape n =
            let
                angle = degrees (30.0 * toNumber n)

            in
                circle 10.0
                |> filled (hsl angle 0.7 0.5)
                |> move (Tuple (45.0 * cos angle) (45.0 * sin angle))


example22 :: Example
example22 =
    Example
        { caption:
            """
            makeCollage 300 300
                ( ( ngon 4 75.0
                    |> filled clearGrey
                    |> moveX -10.0
                  )
                : ( ngon 5 50.0
                    |> filled clearGrey
                    |> moveX 50.0
                    |> moveY 10.0
                  )
                : Nil
                )

            where
                clearGrey =
                    rgba 111 111 111 0.6

            """
        , reference: "StaticCollage/example19.png"
        , collage:
            makeCollage 300 300
                ( ( ngon 4 75.0
                    |> filled clearGrey
                    |> moveX (-10.0)
                  )
                : ( ngon 5 50.0
                    |> filled clearGrey
                    |> moveX 50.0
                    |> moveY 10.0
                  )
                : Nil
                )
        }

    where
        clearGrey =
            rgba 111 111 111 0.6


example23 :: Example
example23 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined defaultLine
                    ( polygon
                        ( (Tuple   50.0    50.0)
                        : (Tuple   50.0  (-50.0))
                        : (Tuple (-50.0) (-50.0))
                        : (Tuple (-50.0)   50.0)
                        : (Tuple   50.0    50.0)
                        : Nil
                        )
                    )
                : Nil
                )
            """
        , reference: "StaticCollage/example23.png"
        , collage:
            makeCollage 100 100
                ( outlined defaultLine
                    ( polygon
                        ( (Tuple   50.0    50.0)
                        : (Tuple   50.0  (-50.0))
                        : (Tuple (-50.0) (-50.0))
                        : (Tuple (-50.0)   50.0)
                        : (Tuple   50.0    50.0)
                        : Nil
                        )
                    )
                : Nil
                )
        }

example24 :: Example
example24 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined defaultLine
                    ( polygon
                        ( (Tuple   50.0    50.0)
                        : (Tuple   50.0  (-50.0))
                        : (Tuple (-50.0) (-50.0))
                        : (Tuple (-50.0)   50.0)
                        : Nil
                        )
                    )
                : Nil
                )
            """
        , reference: "StaticCollage/example24.png"
        , collage:
            makeCollage 100 100
                ( outlined defaultLine
                    ( polygon
                        ( (Tuple   50.0    50.0)
                        : (Tuple   50.0  (-50.0))
                        : (Tuple (-50.0) (-50.0))
                        : (Tuple (-50.0)   50.0)
                        : Nil
                        )
                    )
                : Nil
                )
        }


example25 :: Example
example25 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( moveX 20.0 (outlined defaultLine (square 25.0))
                : Nil
                )
            """
        , reference: "StaticCollage/example25.png"
        , collage:
            makeCollage 100 100
                ( moveX 20.0 (outlined defaultLine (square 25.0))
                : Nil
                )
        }


example26 :: Example
example26 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( moveX 20.0 (outlined defaultLine (oval 25.0 40.0))
                : Nil
                )
            """
        , reference: "StaticCollage/example26.png"
        , collage:
            makeCollage 100 100
                ( moveX 20.0 (outlined defaultLine (oval 25.0 40.0))
                : Nil
                )
        }


example27 :: Example
example27 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( traced (solid red) (segment (Tuple 10.0 40.0) (Tuple 5.0 40.0))
                : Nil
                )
            """
        , reference: "StaticCollage/example27.png"
        , collage:
            makeCollage 100 100
                ( traced (solid red) (segment (Tuple 10.0 40.0) (Tuple 40.0 10.0))
                : Nil
                )
        }


example28 :: Example
example28 =
    Example
        { caption:
            """
            makeCollage 300 300
                ( alpha 0.6 ( ngon 4 75.0
                    # filled clearGrey
                    # move (Tuple (-10.0) 0.0)
                  )
                : alpha 0.6 ( ngon 5 50.0
                    # filled clearGrey
                    # move (Tuple 50.0 10.0)
                  )
                : Nil
                )
            """
        , reference: "StaticCollage/example19.png"
        , collage:
            makeCollage 300 300
                ( alpha 0.6 ( ngon 4 75.0
                    # filled clearGrey
                    # move (Tuple (-10.0) 0.0)
                  )
                : alpha 0.6 ( ngon 5 50.0
                    # filled clearGrey
                    # move (Tuple 50.0 10.0)
                  )
                : Nil
                )
        }

    where
        clearGrey =
            rgb 111 111 111


example29 :: Example
example29 =
    Example
        { caption:
            """
            makeCollage 300 300
                ( moveY   140.0  (text plainText)
                : moveY   120.0  (text biggerText)
                : moveY   100.0  (text redText)
                : moveY    80.0  (text boldText)
                : moveY    60.0  (text italicText)
                : moveY    40.0  (text monospaceText)
                : moveY    20.0  (text underlinedText)
                : moveY     0.0  (text overlinedText)
                : moveY  (-20.0) (text strikethroughText)
                : moveY  (-40.0) (text comboText)
                : moveY  (-60.0) (outlinedText (solid red) biggerText)
                : moveY  (-80.0) (outlinedText (dotted red) biggerText)
                : moveY (-100.0) (outlinedText (dashed red) biggerText)
                : moveY (-120.0) (outlinedText (defaultLine {width = 4.0}) biggerText)

                : ( (text rotatedAndScaled)
                    |> moveX (-140.0)
                    |> scale 2.0
                    |> rotate (degrees 90.0)
                  )
                : Nil
                )
            """
        , reference: "StaticCollage/example29.png"
        , collage:
            makeCollage 300 300
                ( moveY   140.0  (text plainText)
                : moveY   120.0  (text biggerText)
                : moveY   100.0  (text redText)
                : moveY    80.0  (text boldText)
                : moveY    60.0  (text italicText)
                : moveY    40.0  (text monospaceText)
                : moveY    20.0  (text underlinedText)
                : moveY     0.0  (text overlinedText)
                : moveY  (-20.0) (text strikethroughText)
                : moveY  (-40.0) (text comboText)
                : moveY  (-60.0) (outlinedText (solid red) biggerText)
                : moveY  (-80.0) (outlinedText (dotted red) biggerText)
                : moveY (-100.0) (outlinedText (dashed red) biggerText)
                : moveY (-120.0) (outlinedText (defaultLine {width = 4.0}) biggerText)

                : ( (text rotatedAndScaled)
                    |> moveX (-140.0)
                    |> scale 2.0
                    |> rotate (degrees 90.0)
                  )
                : Nil
                )
        }

    where
        typeface =
            Text.typeface ("sans-serif" : Nil)

        plainText =
            typeface $
                Text.fromString "A plain old string"

        rotatedAndScaled =
            typeface $
                Text.fromString "Rotated and scaled"

        biggerText =
            typeface $
                Text.height 20.0 (Text.fromString "Bigger")

        redText =
            typeface $
                Text.color red (Text.fromString "Some red text")

        boldText =
            typeface $
                Text.bold (Text.fromString "Some bold text")

        italicText =
            typeface $
                Text.italic (Text.fromString "Some italic text")

        monospaceText =
            Text.monospace (Text.fromString "Some monospace text")

        underlinedText =
            typeface $
                Text.line Text.Under (Text.fromString "Some underlined text")

        overlinedText =
            typeface $
                Text.line Text.Over (Text.fromString "Some overlined text")

        strikethroughText =
            typeface $
                Text.line Text.Through (Text.fromString "Some strikethrough text")

        comboText =
            typeface $
                Text.join (Text.fromString ", ") (redText : boldText : italicText : Nil)


-- Still to test
--
-- sprite
-- group
-- groupTransform
-- Turning collage into an `Element`
-- groups that mix Collage and Element
