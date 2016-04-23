
-- | Testing `Collage` by itself ... just rendering, no updating.
-- |
-- | For instructions on building and running, see StaticCollage.html in this
-- | directory.

module Examples.Graphics.StaticCollage where


import Elm.Graphics.Collage
import Elm.Graphics.Internal (setStyle)
import Elm.Color (red, linear, radial, rgb, rgba, white, blue, purple, green)
import Elm.Basics ((|>), degrees)

import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (Canvas)
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.Renderable (class Renderable, render, renderIntoDOM, Position(AfterLastChild), defaultUpdate)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..), textToNode, documentToNonElementParentNode)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)

import Prelude (bind, pure, Unit, unit, (>>=), ($), (<$>), negate, (/=), (#))


main :: ∀ e. Eff (canvas :: Canvas, dom :: DOM | e) Unit
main = do
    doc <-
        htmlDocumentToDocument <$>
            (window >>= document)

    nullableContainer <-
        getElementById (ElementId "contents") (documentToNonElementParentNode doc)

    for_ (toMaybe nullableContainer) \container -> do
        table <- elementToNode <$> createElement "table" doc
        tbody <- elementToNode <$> createElement "tbody" doc
        thead <- elementToNode <$> createElement "thead" doc

        appendChild thead table
        appendChild tbody table
        appendChild table (elementToNode container)

        column1 <- elementToNode <$> createElement "th" doc
        column2 <- elementToNode <$> createElement "th" doc
        column3 <- elementToNode <$> createElement "th" doc

        text1 <- textToNode <$> createTextNode "Code" doc
        text2 <- textToNode <$> createTextNode "Result" doc
        text3 <- textToNode <$> createTextNode "Should look like" doc

        appendChild text1 column1
        appendChild text2 column2
        appendChild text3 column3

        appendChild column1 thead
        appendChild column2 thead
        appendChild column3 thead

        for_ examples $
            renderIntoDOM AfterLastChild tbody


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

        setStyle "border" "1px solid blue" column1
        setStyle "border" "1px solid blue" column2
        setStyle "border" "1px solid blue" column3

        appendChild (elementToNode column1) row
        appendChild (elementToNode column2) row
        appendChild (elementToNode column3) row

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
            pure unit

        pure row

    -- Note that I really want
    --
    --     update = defaultUpdate
    --
    -- ... but the compiler complains.
    update rendered = render


examples :: List Example
examples =
    ( example1 : example2 : example3 : example4 : example5 : example6
    : example7 : example8 : example9 : example10 : example11 : example12
    : example13 : example14 : example15 : example16 : example17 : example18
    : example19 : example20
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
                {x: 0.0, y: 60.0}
                {x: 0.0, y: -60.0}
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
                ( move {x: -55.0, y: -55.0} (gradient grad1 (circle 100.0))
                : move {x:  40.0, y:  85.0} (gradient grad2 (circle 100.0))
                : move {x:  50.0, y: -10.0} (gradient grad3 (circle 100.0))
                : move {x: -10.0, y:  50.0} (gradient grad4 (circle 100.0))
                : Nil
                )
        }

    where
        grad1 =
            radial
                {x: 0.0, y:  0.0} 50.0
                {x: 0.0, y: 10.0} 90.0
                ( Tuple 0.0 (rgb  244 242 1)
                : Tuple 0.8 (rgb  228 199 0)
                : Tuple 1.0 (rgba 228 199 0 0.0)
                : Nil
                )

        grad2 =
            radial
                {x: 0.0, y:  0.0} 15.0
                {x: 7.0, y: -5.0} 40.0
                ( Tuple 0.0 (rgb  0 201 255)
                : Tuple 0.8 (rgb  0 181 226)
                : Tuple 1.0 (rgba 0 181 226 0.0)
                : Nil
                )

        grad3 =
            radial
                {x: 0.0, y:   0.0} 20.0
                {x: 7.0, y: -15.0} 50.0
                ( Tuple  0.0 (rgb  255 95 152)
                : Tuple 0.75 (rgb  255 1 136)
                : Tuple  1.0 (rgba 255 1 136 0.0)
                : Nil
                )

        grad4 =
            radial
                {x: 0.0, y:  0.0} 10.0
                {x: 7.0, y: -5.0} 30.0
                ( Tuple 0.0 (rgb  167 211 12)
                : Tuple 0.9 (rgb  1 159 98)
                : Tuple 1.0 (rgba 1 159 98 0.0)
                : Nil
                )


example9 :: Example
example9 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined defaultLine (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example9.png"
        , collage:
            makeCollage 100 100
                ( outlined defaultLine (rect 100.0 50.0)
                : Nil
                )
        }


example10 :: Example
example10 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (defaultLine {color = red}) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example10.png"
        , collage:
            makeCollage 100 100
                ( outlined (defaultLine {color = red}) (rect 100.0 50.0)
                : Nil
                )
        }


example11 :: Example
example11 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (defaultLine {width = 4.0}) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example11.png"
        , collage:
            makeCollage 100 100
                ( outlined (defaultLine {width = 4.0}) (rect 100.0 50.0)
                : Nil
                )
        }


example12 :: Example
example12 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (defaultLine {join = Smooth}) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example12.png"
        , collage:
            makeCollage 100 100
                ( outlined (defaultLine {join = Smooth}) (rect 100.0 50.0)
                : Nil
                )
        }


example13 :: Example
example13 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (defaultLine {join = Clipped}) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example13.png"
        , collage:
            makeCollage 100 100
                ( outlined (defaultLine {join = Clipped}) (rect 100.0 50.0)
                : Nil
                )
        }


example14 :: Example
example14 =
    Example
        { caption:
            """
            makeCollage 100 100
                ( outlined (defaultLine {join = Sharp 3.0}) (rect 100.0 50.0)
                : Nil
                )
            """
        , reference: "StaticCollage/example14.png"
        , collage:
            makeCollage 100 100
                ( outlined (defaultLine {join = Sharp 3.0}) (rect 100.0 50.0)
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
                ( move {x: 0.0, y: -55.0} blueSquare
                : move {x: 0.0, y:  55.0} redSquare
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
                ( {x:  50.0, y:  50.0}
                : {x:  50.0, y: -50.0}
                : {x: -50.0, y: -50.0}
                : {x: -50.0, y:  50.0}
                : {x:  50.0, y:  50.0}
                : Nil
                )


example19 :: Example
example19 =
    Example
        { caption: "http://elm-lang.org/examples/shapes"
        , reference: "staticcollage/example19.png"
        , collage:
            makeCollage 300 300
                ( ( ngon 4 75.0
                    # filled clearGrey
                    # move {x: -10.0, y: 0.0}
                  )
                : ( ngon 5 50.0
                    # filled clearGrey
                    # move {x: 50.0, y: 10.0}
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
                    |> move {x: 100.0, y: 0.0}
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

