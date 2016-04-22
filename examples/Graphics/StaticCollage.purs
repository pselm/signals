
-- | Testing `Collage` by itself ... just rendering, no updating.
-- |
-- | For instructions on building and running, see StaticCollage.html in this
-- | directory.

module Examples.Graphics.StaticCollage where


import Elm.Graphics.Collage
import Elm.Graphics.Internal (setStyle)
import Elm.Color (red, linear, radial, rgb, rgba, white)

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
import DOM.Node.Node (appendChild)

import Prelude (bind, pure, Unit, (>>=), ($), (<$>), negate)


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
        appendChild tbody table
        appendChild table (elementToNode container)

        for_ examples $
            renderIntoDOM AfterLastChild tbody


newtype Example = Example
    { caption :: String
    , collage :: Collage
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
        setStyle "border" "1px solid blue" column1
        setStyle "border" "1px solid blue" column2
        appendChild (elementToNode column1) row
        appendChild (elementToNode column2) row

        caption <- elementToNode <$> createElement "pre" doc
        text <- textToNode <$> createTextNode example.caption doc
        appendChild text caption
        appendChild caption (elementToNode column1)

        collage <- render example.collage
        appendChild collage (elementToNode column2)

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
    : example7 : example8
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
