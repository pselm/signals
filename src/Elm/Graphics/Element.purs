
-- | Graphical elements that snap together to build complex widgets and layouts.
-- | Each Element is a rectangle with a known width and height, making them easy to
-- | combine and position.

module Elm.Graphics.Element
    ( Element
    , image, fittedImage, croppedImage, tiledImage
    , leftAligned, rightAligned, centered, justified, show
    , width, height, size, color, opacity, link, tag
    , widthOf, heightOf, sizeOf
    , flow, Direction, up, down, left, right, inward, outward
    , layers, above, below, beside
    , empty, spacer, container
    , middle, midTop, midBottom, midLeft, midRight, topLeft, topRight
    , bottomLeft, bottomRight
    , Pos, Position
    , absolute, relative, middleAt, midTopAt, midBottomAt, midLeftAt
    , midRightAt, topLeftAt, topRightAt, bottomLeftAt, bottomRightAt
    -- The following aren't exposed by Elm, since the runtime accesses them
    -- magically. Ultimately, we'll probably create a class which would be
    -- implemented here, so that we can swap in Graphics.Element or
    -- a VirtualDOM etc.
    , updateAndReplace, render
    ) where


import Elm.Basics (Float, truncate)
import Elm.Color (Color)
import Elm.List (List(..))
import Data.List (null, (:), reverse, length, index)
import Elm.Maybe (Maybe(..))
import Elm.Text (Text, renderHtml)
import Data.Maybe (fromMaybe)
import Data.Int (round, toNumber)
import Data.Ord (max)
import Data.Foldable (maximum, sum, for_)
import Data.Traversable (for)
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith)
import Data.Array (catMaybes)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToDocument, htmlElementToNode)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Element) as DOM
import DOM.Node.Types (elementToNode, elementToParentNode, elementToEventTarget, ElementId(..))
import DOM.Node.Element (setId, setAttribute, tagName)
import DOM.Node.Node (appendChild, removeChild, parentNode, parentElement, replaceChild)
import DOM.Node.ParentNode (firstElementChild, children) as ParentNode
import DOM.Node.HTMLCollection (length, item) as HTMLCollection
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.EventTypes (load)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad (when, unless)
import Text.Format (format, precision)

import Prelude
    ( class Show, class Eq, Unit, unit
    , flip, map, (<$>), ($), (>>>)
    , bind, (>>=), pure
    , (+), (-), (/), (*), (<>)
    , (==), (/=), (>), (||), (&&), negate
    )


-- FOREIGN

-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: ∀ e. String -> String -> DOM.Element -> Eff (dom :: DOM | e) Unit

-- Removes the style
foreign import removeStyle :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit

-- Inner HTML
foreign import setInnerHtml :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit

-- Dimensions
foreign import getDimensions :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) {width :: Int, height :: Int}

-- Image width
foreign import getImageWidth :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) Int

-- Image height
foreign import getImageHeight :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) Int

-- Unsafe document
foreign import nullableDocument :: ∀ e. Eff (dom :: DOM | e) (Nullable HTMLDocument)

-- Whether one thing is the same as another ... that is, the identical thing, not equality.
-- Note that we allow the types to differ, since the compiler might think of them as different
-- types for one reason or another.
foreign import same :: ∀ a b. a -> b -> Boolean

-- This should probably be in DOM.Node.Element ... remember to suggest that.
foreign import removeAttribute :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit


-- PRIMITIVES

-- | A graphical element that can be rendered on screen. Every element is a
-- | rectangle with a known width and height, so they can be composed and stacked
-- | easily.
newtype Element = Element
    { props :: Properties
    , element :: ElementPrim
    }

instance eqElement :: Eq Element where
    eq (Element a) (Element b) =
        (eqProperties a.props b.props) && a.element == b.element


-- I've removed the `id :: Int` because it's essentially effectful to add an id.
-- It seems to be used (indirectly) to determine whether two ElementPrim's are
-- the same object. We'll use `same` for that instead.
type Properties =
    { width :: Int
    , height :: Int
    , opacity :: Float
    , color :: Maybe Color
    , href :: String
    , tag :: String
    -- TODO: Figure out hover / click logic.
    -- , hover :: Unit
    -- , click :: Unit
    }

eqProperties :: Properties -> Properties -> Boolean
eqProperties a b =
    a.width == b.width &&
    a.height == b.height &&
    a.opacity == b.opacity &&
    a.color == b.color &&
    a.href == b.href &&
    a.tag == b.tag


data ElementPrim
    = Image ImageStyle Int Int String
    | Container RawPosition Element
    | Flow Direction (List Element)
    | Spacer
    | RawHtml String String -- html align
    | Custom -- for custom Elements implemented in JS, see collage for example

instance eqElementPrim :: Eq ElementPrim where
    eq (Image style1 width1 height1 src1) (Image style2 width2 height2 src2) =
        style1 == style2 && width1 == width2 && height1 == height2 && src1 == src2

    eq (Container pos1 elem1) (Container pos2 elem2) =
        (eqRawPosition pos1 pos2) && elem1 == elem2

    eq (Flow dir1 list1) (Flow dir2 list2) =
        dir1 == dir2 && list1 == list2

    eq Spacer Spacer = true

    eq (RawHtml html1 align1) (RawHtml html2 align2) =
        align1 == align2 && html1 == html2

    -- For now ... revisit later!
    eq Custom Custom = true

    eq _ _ = false


data ImageStyle
    = Plain
    | Fitted
    | Cropped {top :: Int, left :: Int}
    | Tiled

instance eqImageStyle :: Eq ImageStyle where
    eq Plain Plain = true
    eq Fitted Fitted = true
    eq (Cropped rec1) (Cropped rec2) = rec1.top == rec2.top && rec1.left == rec2.left
    eq Tiled Tiled = true
    eq _ _ = false


-- | Specifies a position for an element within a `container`, like “the top
-- | left corner”.
newtype Position = Position RawPosition

instance eqPosition :: Eq Position where
    eq (Position a) (Position b) = eqRawPosition a b


-- | Specifies a distance from a particular location within a `container`, like
-- | “20 pixels right and up from the center”. You can use `absolute` or `relative`
-- | to specify a `Pos` in pixels or as a percentage of the container.
data Pos
    = Absolute Int
    | Relative Float

instance eqPos :: Eq Pos where
    eq (Absolute a) (Absolute b) = a == b
    eq (Relative a) (Relative b) = a == b
    eq _ _ = false


data Three = P | Z | N

instance eqThree :: Eq Three where
    eq P P = true
    eq Z Z = true
    eq N N = true
    eq _ _ = false


type RawPosition =
    { horizontal :: Three
    , vertical :: Three
    , x :: Pos
    , y :: Pos
    }

eqRawPosition :: RawPosition -> RawPosition -> Boolean
eqRawPosition a b =
    a.horizontal == b.horizontal && a.vertical == b.vertical && a.x == b. x && a.y == b.y 


-- | Represents a `flow` direction for a list of elements.
data Direction
    = DUp
    | DDown
    | DLeft
    | DRight
    | DIn
    | DOut

instance eqDirection :: Eq Direction where
    eq DUp DUp = true
    eq DDown DDown = true
    eq DLeft DLeft = true
    eq DRight DRight = true
    eq DIn DIn = true
    eq DOut DOut = true
    eq _ _ = false


-- | An Element that takes up no space. Good for things that appear conditionally:
-- |
-- |     flow down [ img1, if showMore then img2 else empty ]
empty :: Element
empty = spacer 0 0


-- | Get the width of an Element
widthOf :: Element -> Int
widthOf (Element {props}) = props.width


-- | Get the height of an Element
heightOf :: Element -> Int
heightOf (Element {props}) = props.height


-- | Get the width and height of an Element
sizeOf :: Element -> {width :: Int, height :: Int}
sizeOf (Element {props}) =
    { width: props.width
    , height: props.height
    }


-- | Create an `Element` with a given width.
width :: Int -> Element -> Element
width newWidth (Element {element, props}) =
    let
        newHeight =
            case element of
                Image _ w h _ ->
                    round (toNumber h / toNumber w * toNumber newWidth)

                RawHtml html _ ->
                    _.height $ runHtmlHeight newWidth html

                _ ->
                    props.height

    in
        Element
            { element
            , props: props
                { width = newWidth
                , height = newHeight
                }
            }


-- | Create an `Element` with a given height.
height :: Int -> Element -> Element
height newHeight (Element {element, props}) =
    Element
        { element
        , props: props { height = newHeight }
        }


-- | Create an `Element` with a new width and height.
size :: Int -> Int -> Element -> Element
size w h e = do
    height h (width w e)


-- | Create an `Element` with a given opacity. Opacity is a number between 0 and 1
-- | where 0 means totally clear.
opacity :: Float -> Element -> Element
opacity givenOpacity (Element {element, props}) =
    Element
        { element
        , props: props { opacity = givenOpacity }
        }


-- | Create an `Element` with a given background color.
color :: Color -> Element -> Element
color clr (Element {element, props}) =
    Element
        { element
        , props: props { color = Just clr }
        }


-- | Create an `Element` with a tag. This lets you link directly to it.
-- | The element `(tag "all-about-badgers" thirdParagraph)` can be reached
-- | with a link like this: `/facts-about-animals.elm#all-about-badgers`
tag :: String -> Element -> Element
tag name (Element {element, props}) =
    Element
        { element
        , props: props { tag = name }
        }


-- | Create an `Element` that is a hyper-link.
link :: String -> Element -> Element
link href (Element {element, props}) =
    Element
        { element
        , props: props { href = href }
        }


-- IMAGES

-- | Create an image given a width, height, and image source.
image :: Int -> Int -> String -> Element
image w h src =
    newElement w h (Image Plain w h src)


-- | Create a fitted image given a width, height, and image source.
-- | This will crop the picture to best fill the given dimensions.
fittedImage :: Int -> Int -> String -> Element
fittedImage w h src =
    newElement w h (Image Fitted w h src)


-- | Create a cropped image. Take a rectangle out of the picture starting
-- | at the given top left coordinate. If you have a 140-by-140 image,
-- | the following will cut a 100-by-100 square out of the middle of it.
-- |
-- |     croppedImage {top: 20, left: 20} 100 100 "yogi.jpg"
croppedImage :: {top :: Int, left :: Int} -> Int -> Int -> String -> Element
croppedImage pos w h src =
    newElement w h (Image (Cropped pos) w h src)


-- | Create a tiled image. Repeat the image to fill the given width and height.
-- |
-- |     tiledImage 100 100 "yogi.jpg"
tiledImage :: Int -> Int -> String -> Element
tiledImage w h src =
    newElement w h (Image Tiled w h src)


-- TEXT

-- | Align text along the left side of the text block. This is sometimes known as
-- | *ragged right*.
leftAligned :: Text -> Element
leftAligned = block "left"


-- | Align text along the right side of the text block. This is sometimes known
-- | as *ragged left*.
rightAligned :: Text -> Element
rightAligned = block "right"


-- | Center text in the text block. There is equal spacing on either side of a
-- | line of text.
centered :: Text -> Element
centered = block "center"


-- | Align text along the left and right sides of the text block. Word spacing is
-- | adjusted to make this possible.
justified :: Text -> Element
justified = block "justify"


-- | Convert anything to its textual representation and make it displayable in
-- | the browser. Excellent for debugging.
-- |
-- |     main :: Element
-- |     main =
-- |       show "Hello World!"
-- |
-- |     show value =
-- |         leftAligned (Text.monospace (Text.fromString (toString value)))
show :: ∀ a. (Show a) => a -> Element
show = Prelude.show >>> Elm.Text.fromString >>> Elm.Text.monospace >>> leftAligned


-- LAYOUT

-- | Put an element in a container. This lets you position the element really
-- | easily, and there are tons of ways to set the `Position`.
-- | To center `element` exactly in a 300-by-300 square you would say:
-- |
-- |     container 300 300 middle element
-- |
-- | By setting the color of the container, you can create borders.
container :: Int -> Int -> Position -> Element -> Element
container w h (Position rawPos) e =
    newElement w h (Container rawPos e)


-- | Create an empty box. This is useful for getting your spacing right and
-- | for making borders.
spacer :: Int -> Int -> Element
spacer w h =
    newElement w h Spacer


-- | Have a list of elements flow in a particular direction.
-- | The `Direction` starts from the first element in the list.
-- |
-- |     flow right [a,b,c]
-- |
-- |         +---+---+---+
-- |         | a | b | c |
-- |         +---+---+---+
flow :: Direction -> List Element -> Element
flow dir es =
    let
        ws = map widthOf es
        hs = map heightOf es
        maxOrZero list = fromMaybe 0 (maximum list)
        newFlow w h = newElement w h (Flow dir es)

    in
        if null es
            then empty
            else case dir of
                DUp    -> newFlow (maxOrZero ws) (sum hs)
                DDown  -> newFlow (maxOrZero ws) (sum hs)
                DLeft  -> newFlow (sum ws) (maxOrZero hs)
                DRight -> newFlow (sum ws) (maxOrZero hs)
                DIn    -> newFlow (maxOrZero ws) (maxOrZero hs)
                DOut   -> newFlow (maxOrZero ws) (maxOrZero hs)


-- | Stack elements vertically.
-- | To put `a` above `b` you would say: ``a `above` b``
above :: Element -> Element -> Element
above hi lo =
    newElement
        (max (widthOf hi) (widthOf lo))
        (heightOf hi + heightOf lo)
        (Flow DDown (hi : lo : Nil))


-- | Stack elements vertically.
-- | To put `a` below `b` you would say: ``a `below` b``
below :: Element -> Element -> Element
below = flip above


-- | Put elements beside each other horizontally.
-- | To put `a` beside `b` you would say: ``a `beside` b``
beside :: Element -> Element -> Element
beside lft rht =
    newElement
        (widthOf lft + widthOf rht)
        (max (heightOf lft) (heightOf rht))
        (Flow right (lft : rht : Nil))


-- | Layer elements on top of each other, starting from the bottom:
-- | `layers == flow outward`
layers :: List Element -> Element
layers es =
    let
        ws = map widthOf es
        hs = map heightOf es

    in
        newElement
            (fromMaybe 0 (maximum ws))
            (fromMaybe 0 (maximum hs))
            (Flow DOut es)


-- Repetitive things --

-- | A position specified in pixels. If you want something 10 pixels to the
-- | right of the middle of a container, you would write this:
-- |
-- |     middleAt (absolute 10) (absolute 0)
absolute :: Int -> Pos
absolute = Absolute


-- | A position specified as a percentage. If you want something 10% away from
-- | the top left corner, you would say:
-- |
-- }     topLeftAt (relative 0.1) (relative 0.1)
relative :: Float -> Pos
relative = Relative


middle :: Position
middle =
    Position
        { horizontal: Z
        , vertical: Z
        , x: Relative 0.5
        , y: Relative 0.5
        }


topLeft :: Position
topLeft =
    Position
        { horizontal: N
        , vertical: P
        , x: Absolute 0
        , y: Absolute 0
        }


topRight :: Position
topRight =
    Position
        { horizontal: P
        , vertical: P
        , x: Absolute 0
        , y: Absolute 0
        }


bottomLeft :: Position
bottomLeft =
    Position
        { horizontal: N
        , vertical: N
        , x: Absolute 0
        , y: Absolute 0
        }


bottomRight :: Position
bottomRight =
    Position
        { horizontal: P
        , vertical: N
        , x: Absolute 0
        , y: Absolute 0
        }


midLeft :: Position
midLeft =
    Position
        { horizontal: N
        , vertical: Z
        , x: Absolute 0
        , y: Relative 0.5
        }


midRight :: Position
midRight =
    Position
        { horizontal: P
        , vertical: Z
        , x: Absolute 0
        , y: Relative 0.5
        }


midTop :: Position
midTop =
    Position
        { horizontal: Z
        , vertical: P
        , x: Relative 0.5
        , y: Absolute 0
        }


midBottom :: Position
midBottom =
    Position
        { horizontal: Z
        , vertical: N
        , x: Relative 0.5
        , y: Absolute 0
        }


middleAt :: Pos -> Pos -> Position
middleAt x y =
    Position
        { horizontal: Z
        , vertical: Z
        , x
        , y
        }


topLeftAt :: Pos -> Pos -> Position
topLeftAt x y =
    Position
        { horizontal: N
        , vertical: P
        , x
        , y
        }


topRightAt :: Pos -> Pos -> Position
topRightAt x y =
    Position
        { horizontal: P
        , vertical: P
        , x
        , y
        }


bottomLeftAt :: Pos -> Pos -> Position
bottomLeftAt x y =
    Position
        { horizontal: N
        , vertical: N
        , x
        , y
        }


bottomRightAt :: Pos -> Pos -> Position
bottomRightAt x y =
    Position
        { horizontal: P
        , vertical: N
        , x
        , y
        }


midLeftAt :: Pos -> Pos -> Position
midLeftAt x y =
    Position
        { horizontal: N
        , vertical: Z
        , x
        , y
        }


midRightAt :: Pos -> Pos -> Position
midRightAt x y =
    Position
        { horizontal: P
        , vertical: Z
        , x
        , y
        }


midTopAt :: Pos -> Pos -> Position
midTopAt x y =
    Position
        { horizontal: Z
        , vertical: P
        , x
        , y
        }


midBottomAt :: Pos -> Pos -> Position
midBottomAt x y =
    Position
        { horizontal: Z
        , vertical: N
        , x
        , y
        }


up :: Direction
up = DUp


down :: Direction
down = DDown


left :: Direction
left = DLeft


right :: Direction
right = DRight


inward :: Direction
inward = DIn


outward :: Direction
outward = DOut


-- The remainder is a conversion of the Native JS code from Elm

-- CREATION

createNode :: ∀ e. String -> Eff (dom :: DOM | e) DOM.Element
createNode elementType = do
    node <- window >>= document >>= htmlDocumentToDocument >>> createElement elementType
    setStyle "padding" "0px" node
    setStyle "margin" "0px" node
    pure node


newElement :: Int -> Int -> ElementPrim -> Element
newElement w h prim =
    Element
        { element: prim
        , props:
            { width: w
            , height: h
            , opacity: 1.0
            , color: Nothing
            , href: ""
            , tag: ""
            }
        }


-- PROPERTIES

setProps :: ∀ e. Element -> DOM.Element -> Eff (dom :: DOM | e) DOM.Element
setProps (Element {props, element}) node = do
    let
        w = props.width
        h = props.height

        -- TODO: These are concepts from Input.elm ... revisit when I do that.
        -- var width = props.width - (element.adjustWidth || 0);
        -- var height = props.height - (element.adjustHeight || 0);

    setStyle "width" (Prelude.show w <> "px") node
    setStyle "height" (Prelude.show h <> "px") node

    when (props.opacity /= 1.0) $
        setStyle "opacity" (Prelude.show props.opacity) node

    for props.color \c ->
        setStyle "backgroundColor" (Elm.Color.toCss c) node

    when (props.tag /= "") $
        setId (ElementId props.tag) node

    -- TODO: Figure out hover and click
    {-
    if (props.hover.ctor !== '_Tuple0')
    {
        addHover(node, props.hover);
    }

    if (props.click.ctor !== '_Tuple0')
    {
        addClick(node, props.click);
    }
    -}

    if props.href == ""
        then pure node
        else do
            anchor <- createNode "a"
            setAttribute "href" props.href anchor
            setStyle "display" "block" anchor
            setStyle "pointerEvents" "auto" anchor
            appendChild (elementToNode node) (elementToNode anchor)
            pure anchor


{- TODO
    function addClick(e, handler)
    {
        e.style.pointerEvents = 'auto';
        e.elm_click_handler = handler;
        function trigger(ev)
        {
            e.elm_click_handler(Utils.Tuple0);
            ev.stopPropagation();
        }
        e.elm_click_trigger = trigger;
        e.addEventListener('click', trigger);
    }

    function removeClick(e, handler)
    {
        if (e.elm_click_trigger)
        {
            e.removeEventListener('click', e.elm_click_trigger);
            e.elm_click_trigger = null;
            e.elm_click_handler = null;
        }
    }

    function addHover(e, handler)
    {
        e.style.pointerEvents = 'auto';
        e.elm_hover_handler = handler;
        e.elm_hover_count = 0;

        function over(evt)
        {
            if (e.elm_hover_count++ > 0) return;
            e.elm_hover_handler(true);
            evt.stopPropagation();
        }
        function out(evt)
        {
            if (e.contains(evt.toElement || evt.relatedTarget)) return;
            e.elm_hover_count = 0;
            e.elm_hover_handler(false);
            evt.stopPropagation();
        }
        e.elm_hover_over = over;
        e.elm_hover_out = out;
        e.addEventListener('mouseover', over);
        e.addEventListener('mouseout', out);
    }

    function removeHover(e)
    {
        e.elm_hover_handler = null;
        if (e.elm_hover_over)
        {
            e.removeEventListener('mouseover', e.elm_hover_over);
            e.elm_hover_over = null;
        }
        if (e.elm_hover_out)
        {
            e.removeEventListener('mouseout', e.elm_hover_out);
            e.elm_hover_out = null;
        }
    }
-}


-- IMAGES

makeImage :: ∀ e. Properties -> ImageStyle -> Int -> Int -> String -> Eff (dom :: DOM | e) DOM.Element
makeImage props imageStyle imageWidth imageHeight src =
    case imageStyle of
        Plain -> do
            img <- createNode "img"
            setAttribute "src" src img
            setAttribute "name" src img
            setStyle "display" "block" img
            pure img

        Fitted -> do
            div <- createNode "div"
            let s = "url('" <> src <> "') no-repeat center"
            setStyle "background" s div
            setStyle "webkitBackgroundSize" "cover" div
            setStyle "MozBackgroundSize" "cover" div
            setStyle "OBackgroundSize" "cover" div
            setStyle "backgroundSize" "cover" div
            pure div

        Cropped pos -> do
            e <- createNode "div"
            setStyle "overflow" "hidden" e

            img <- createNode "img"

            let
                listener =
                    eventListener \event -> do
                        intrinsicWidth <- toNumber <$> getImageWidth img
                        intrinsicHeight <- toNumber <$> getImageHeight img

                        let
                            sw =
                                toNumber props.width / toNumber imageWidth

                            sh =
                                toNumber props.height / toNumber imageHeight

                            newWidth =
                                Prelude.show (truncate (intrinsicWidth * sw)) <> "px"

                            newHeight =
                                Prelude.show (truncate (intrinsicHeight * sh)) <> "px"

                            marginLeft =
                                Prelude.show (truncate (toNumber (-pos.left) * sw)) <> "px"

                            marginTop =
                                Prelude.show (truncate (toNumber (-pos.top) * sh)) <> "px"

                        setStyle "width" newWidth img
                        setStyle "height" newHeight img
                        setStyle "marginLeft" marginLeft img
                        setStyle "marginTop" marginTop img

                        pure unit

            addEventListener load listener false (elementToEventTarget img)
            setAttribute "src" src img
            setAttribute "name" src img
            appendChild (elementToNode img) (elementToNode e)
            pure e

        Tiled -> do
            div <- createNode "div"
            let s = "url(" <> src <> ")"
            setStyle "backgroundImage" s div
            pure div


-- FLOW

goOut :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) DOM.Element
goOut node = do
    setStyle "position" "absolute" node
    pure node


goDown :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) DOM.Element
goDown = pure


goRight :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) DOM.Element
goRight node = do
    setStyle "styleFloat" "left" node
    setStyle "cssFloat" "left" node
    pure node


directionTable :: Direction -> (∀ e. DOM.Element -> Eff (dom :: DOM | e) DOM.Element)
directionTable dir =
    case dir of
        DUp -> goDown
        DDown -> goDown
        DLeft -> goRight
        DRight -> goRight
        DIn -> goOut
        DOut -> goOut


needsReversal :: Direction -> Boolean
needsReversal DUp = true
needsReversal DLeft = true
needsReversal DIn = true
needsReversal _ = false


makeFlow :: ∀ e. Direction -> List Element -> Eff (dom :: DOM | e) DOM.Element
makeFlow dir elist = do
    parent <- createNode "div"

    case dir of
        DIn -> setStyle "pointerEvents" "none" parent
        DOut -> setStyle "pointerEvents" "none" parent
        _ -> pure unit

    let
        possiblyReversed =
            if needsReversal dir
                then reverse elist
                else elist

        goDir =
            directionTable dir

    for possiblyReversed \elem -> do
        rendered <- render elem >>= goDir
        appendChild (elementToNode rendered) (elementToNode parent)

    pure parent


-- CONTAINER

toPos :: Pos -> String
toPos (Absolute pos) = (Prelude.show pos) <> "px"
toPos (Relative pos) = format (precision 2) (pos * 100.0) <> "%"


setPos :: ∀ e. RawPosition -> Element -> DOM.Element -> Eff (dom :: DOM | e) DOM.Element
setPos pos (Element {element, props}) e = do
    let
        w = props.width
        h = props.height

        -- TODO
        -- var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
        -- var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

    setStyle "position" "absolute" e
    setStyle "margin" "auto" e

    case pos.horizontal of
        P -> do
            setStyle "right" (toPos pos.x) e
            removeStyle "left" e

        _ -> do
            setStyle "left" (toPos pos.x) e
            removeStyle "right" e

    case pos.vertical of
        N -> do
            setStyle "bottom" (toPos pos.y) e
            removeStyle "top" e

        _ -> do
            setStyle "top" (toPos pos.y) e
            removeStyle "bottom" e

    let
        translateX =
            case pos.horizontal of
                Z -> Just $ "translateX(" <> Prelude.show ((-w) / 2) <> "px)"
                _ -> Nothing

        translateY =
            case pos.vertical of
                Z -> Just $ "translateY(" <> Prelude.show ((-h) / 2) <> "px)"
                _ -> Nothing

        transform =
            joinWith " " $ catMaybes [translateX, translateY]

    if transform == ""
        then removeTransform e
        else addTransform transform e

    pure e


addTransform :: ∀ e. String -> DOM.Element -> Eff (dom :: DOM | e) Unit
addTransform transform node = do
    setStyle "transform" transform node
    setStyle "msTransform" transform node
    setStyle "MozTransform" transform node
    setStyle "webkitTransform" transform node
    setStyle "OTransform" transform node


removeTransform :: ∀ e. DOM.Element -> Eff (dom :: DOM | e) Unit
removeTransform node = do
    removeStyle "transform" node
    removeStyle "msTransform" node
    removeStyle "MozTransform" node
    removeStyle "webkitTransform" node
    removeStyle "OTransform" node


makeContainer :: ∀ e. RawPosition -> Element -> Eff (dom :: DOM | e) DOM.Element
makeContainer pos elem = do
    e <- render elem
    setPos pos elem e

    div <- createNode "div"
    setStyle "position" "relative" div
    setStyle "overflow" "hidden" div

    appendChild (elementToNode e) (elementToNode div)
    pure div


rawHtml :: ∀ e. String -> String -> Eff (dom :: DOM | e) DOM.Element
rawHtml html align = do
    div <- createNode "div"

    setInnerHtml html div
    setStyle "visibility" "hidden" div

    when (align /= "") $
        setStyle "textAlign" align div

    setStyle "visibility" "visible" div
    setStyle "pointerEvents" "auto" div

    pure div


-- RENDER

render :: ∀ e. Element -> Eff (dom :: DOM | e) DOM.Element
render e = makeElement e >>= setProps e


makeElement :: ∀ e. Element -> Eff (dom :: DOM | e) DOM.Element
makeElement (Element {element, props}) =
    case element of
        Image imageStyle imageWidth imageHeight src ->
            makeImage props imageStyle imageWidth imageHeight src

        Flow direction children ->
            makeFlow direction children

        Container position inner ->
            makeContainer position inner

        Spacer ->
            createNode "div"

        RawHtml html align ->
            rawHtml html align

        Custom ->
            createNode "div"
            -- TODO:
            -- return elem.render(elem.model);


-- UPDATE

updateAndReplace :: ∀ e. DOM.Element -> Element -> Element -> Eff (dom :: DOM | e) DOM.Element
updateAndReplace node curr next = do
    newNode <- update node curr next

    unless (same newNode node) do
        nullableParent <- parentNode (elementToNode node)
        for_ (toMaybe nullableParent) \parent ->
            replaceChild (elementToNode newNode) (elementToNode node) parent

    pure newNode


update :: ∀ e. DOM.Element -> Element -> Element -> Eff (dom :: DOM | e) DOM.Element
update outerNode (Element curr) (Element next) = do
    innerNode <-
        if tagName outerNode == "A"
            then do
                nullableChild <- ParentNode.firstElementChild (elementToParentNode outerNode)
                case toMaybe nullableChild of
                     Just child -> pure child
                     Nothing -> pure outerNode

            else pure outerNode

    let
        nextE = next.element
        currE = curr.element

    if same currE nextE
        then do
            -- If the ElementPrim is the same, then just update the  props
            updateProps innerNode (Element curr) (Element next)
            pure outerNode

        else
            -- Otherwise, it depends on what the old and new element are
            case { nextE, currE } of

                -- Both spacers
                { nextE: Spacer
                , currE: Spacer
                } -> do
                    updateProps innerNode (Element curr) (Element next)
                    pure outerNode

                -- Both RawHtml
                { nextE: RawHtml html _
                , currE: RawHtml oldHtml _
                } -> do
                    when (html /= oldHtml) $
                        setInnerHtml html innerNode

                    updateProps innerNode (Element curr) (Element next)
                    pure outerNode

                -- Both Images
                { nextE: Image imageStyle imageWidth imageHeight src
                , currE: Image oldImageStyle oldImageWidth oldImageHeight oldSrc
                } ->
                    case { imageStyle, oldImageStyle } of
                        -- If we're transitioning from plain to plain, then we just
                        -- have to update the src if necessary, and the props. At
                        -- least, that's how Elm does it.
                        { imageStyle: Plain
                        , oldImageStyle: Plain
                        } -> do
                            when (oldSrc /= src) $
                                setAttribute "src" src innerNode

                            updateProps innerNode (Element curr) (Element next)
                            pure outerNode

                        _ ->
                            -- Width and height changes appear to need a re-render ...
                            -- Normally just an update props would be required.
                            if next.props.width /= curr.props.width ||
                               next.props.height /= curr.props.height ||
                               nextE /= currE
                                    then render (Element next)
                                    else do
                                        updateProps innerNode (Element curr) (Element next)
                                        pure outerNode

                -- Both flows
                { nextE: Flow dir list
                , currE: Flow oldDir oldList
                } -> do
                    if dir /= oldDir
                        then render (Element next)
                        else do
                            kids <- ParentNode.children (elementToParentNode innerNode)
                            len <- HTMLCollection.length kids

                            if len /= length list || len /= length oldList
                                then render (Element next)
                                else do
                                    let
                                        reversal = needsReversal dir
                                        goDir = directionTable dir

                                    -- Why doesn't forE take an Int?
                                    forE 0.0 (toNumber len) \num -> do
                                        let
                                            i = truncate num

                                            kidIndex =
                                                if reversal
                                                    then len - (i + 1)
                                                    else i

                                        kid <- toMaybe <$> HTMLCollection.item kidIndex kids
                                        innerOld <- pure $ index oldList i
                                        innerNext <- pure $ index list i

                                        for_ kid \k ->
                                            for_ innerOld \old ->
                                                for_ innerNext \next ->
                                                    updateAndReplace k old next >>= goDir

                                    updateProps innerNode (Element curr) (Element next)
                                    pure outerNode

                -- Both containers
                { nextE: Container rawPos elem
                , currE: Container oldRawPos oldElem
                } -> do
                    nullableSubnode <- ParentNode.firstElementChild (elementToParentNode innerNode)
                    for_ (toMaybe nullableSubnode) \subnode -> do
                        newSubNode <- updateAndReplace subnode oldElem elem
                        setPos rawPos elem newSubNode

                    updateProps innerNode (Element curr) (Element next)
                    pure outerNode

                -- Both custom
                { nextE: Custom
                , currE: Custom
                } ->
                    render (Element next)
                    -- TODO
                    {-
                        if (currE.type === nextE.type)
                        {
                            var updatedNode = nextE.update(node, currE.model, nextE.model);
                            updateProps(updatedNode, curr, next);
                            return updatedNode;
                        }
                    -}

                -- Different element constructors
                _ ->
                    render (Element next)


updateProps :: ∀ e. DOM.Element -> Element -> Element -> Eff (dom :: DOM | e) Unit
updateProps node (Element curr) (Element next) = do
    let
        nextProps = next.props
        currProps = curr.props

        element = next.element

        w = nextProps.width
        h = nextProps.height

        -- TODO
        -- var width = nextProps.width - (element.adjustWidth || 0);
        -- var height = nextProps.height - (element.adjustHeight || 0);

    when (w /= currProps.width) $
        setStyle "width" ((Prelude.show w) <> "px") node

    when (h /= currProps.height) $
        setStyle "height" ((Prelude.show h) <> "px") node

    when (nextProps.opacity /= currProps.opacity) $
        setStyle "opacity" (Prelude.show nextProps.opacity) node

    when (nextProps.color /= currProps.color) $
        case nextProps.color of
            Just c -> setStyle "backgroundColor" (Elm.Color.toCss c) node
            Nothing -> removeStyle "backgroundColor" node

    when (nextProps.tag /= currProps.tag) $
        if nextProps.tag == ""
            then removeAttribute "id" node
            else setId (ElementId nextProps.tag) node

    when (nextProps.href /= currProps.href) $
        if currProps.href == ""
            then do
                anchor <- createNode "a"
                setAttribute "href" nextProps.href anchor
                setStyle "display" "block" anchor
                setStyle "pointerEvents" "auto" anchor

                nullableParent <- parentNode (elementToNode node)
                for_ (toMaybe nullableParent) \parent -> do
                    replaceChild (elementToNode anchor) (elementToNode node) parent
                    appendChild (elementToNode node) (elementToNode anchor)

            else do
                nullableAnchor <- parentElement (elementToNode node)
                for_ (toMaybe nullableAnchor) \anchor ->
                    if nextProps.href == ""
                        then do
                            nullableParent <- parentNode (elementToNode anchor)
                            for_ (toMaybe nullableParent) \parent ->
                                replaceChild (elementToNode node) (elementToNode anchor) parent

                        else
                            setAttribute "href" nextProps.href anchor

        -- TODO
        {-
        -- update click and hover handlers
        var removed = false;

        // update hover handlers
        if (currProps.hover.ctor === '_Tuple0')
        {
            if (nextProps.hover.ctor !== '_Tuple0')
            {
                addHover(node, nextProps.hover);
            }
        }
        else
        {
            if (nextProps.hover.ctor === '_Tuple0')
            {
                removed = true;
                removeHover(node);
            }
            else
            {
                node.elm_hover_handler = nextProps.hover;
            }
        }

        // update click handlers
        if (currProps.click.ctor === '_Tuple0')
        {
            if (nextProps.click.ctor !== '_Tuple0')
            {
                addClick(node, nextProps.click);
            }
        }
        else
        {
            if (nextProps.click.ctor === '_Tuple0')
            {
                removed = true;
                removeClick(node);
            }
            else
            {
                node.elm_click_handler = nextProps.click;
            }
        }

        // stop capturing clicks if
        if (removed
            && nextProps.hover.ctor === '_Tuple0'
            && nextProps.click.ctor === '_Tuple0')
        {
            node.style.pointerEvents = 'none';
        }
        -}


-- TEXT

block :: String -> Text -> Element
block align text =
    let
        html = renderHtml text
        pos = runHtmlHeight 0 html

    in
        newElement pos.width pos.height $
            RawHtml html align


markdown :: String -> Element
markdown text =
    let
        pos = runHtmlHeight 0 text

    in
        newElement pos.width pos.height $
            RawHtml text ""


-- Calculate htmlHeight without the effect. The theory is that because htmlHeight
-- puts the div in the DOM but then immediately takes it out again, it's reasonable
-- to run this "unsafely" -- the effect is undone. And, of course, it makes the
-- API work as Elm expects it to.
runHtmlHeight :: Int -> String -> {width :: Int, height :: Int}
runHtmlHeight w html =
    unsafePerformEff $ htmlHeight w html


htmlHeight :: ∀ e. Int -> String -> Eff (dom :: DOM | e) {width :: Int, height :: Int}
htmlHeight w html = do
    -- Because of runHtmlHeight, we double-check whether we really have a document or not.
    nullableDoc <- nullableDocument
    case toMaybe nullableDoc of
        Nothing ->
            pure
                { width: 0
                , height: 0
                }

        Just doc -> do
            temp <- createElement "div" (htmlDocumentToDocument doc)
            let tempNode = elementToNode temp

            setInnerHtml html temp

            when (w > 0) $
                setStyle "width" ((Prelude.show w) <> "px") temp

            setStyle "visibility" "hidden" temp
            setStyle "styleFloat" "left" temp
            setStyle "cssFloat" "left" temp

            nullableBody <- body doc

            case toMaybe nullableBody of
                Just b -> do
                    let bodyDoc = htmlElementToNode b
                    appendChild tempNode bodyDoc
                    dim <- getDimensions temp
                    removeChild tempNode bodyDoc
                    pure dim

                Nothing ->
                    pure
                        { width: 0
                        , height: 0
                        }


