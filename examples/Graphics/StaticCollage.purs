
-- | Testing `Collage` by itself ... just rendering, no updating.
-- |
-- | For instructions on building and running, see StaticCollage.html in this
-- | directory.

module Examples.Graphics.StaticCollage where


import Elm.Graphics.Collage
import Elm.Graphics.Internal (setStyle)
import Elm.Color (red)

import Control.Monad.Eff (Eff)
import Graphics.Canvas (Canvas)
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.List (List(..), (:))

import DOM (DOM)
import DOM.Renderable (class Renderable, render, renderIntoDOM, Position(AfterLastChild), defaultUpdate)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (elementToNode, ElementId(..), textToNode, documentToNonElementParentNode)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Node (appendChild)

import Prelude (bind, pure, Unit, (>>=), ($), (<$>))


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
    ( example1 : example2 : example3 : example4 : example5
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
