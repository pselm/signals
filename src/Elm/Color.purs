
-- | Library for working with colors. Includes
-- | [RGB](https://en.wikipedia.org/wiki/RGB_color_model) and
-- | [HSL](http://en.wikipedia.org/wiki/HSL_and_HSV) creation, gradients, and
-- | built-in names.
-- |
-- | The Purescript implementation is built on top of the purescript-colors module
-- | and its `Color` type. So, you can also use functions from that module.

module Elm.Color
    ( module Virtual
    , hsl, hsla
    , greyscale, grayscale, complement
    , toRgb, toHsl, toCss
    , Gradient, linear, radial, toCanvasGradient

    , red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    ) where


-- For re-export

import Color (Color, rgb, rgba) as Virtual

-- For internal use

import Control.Monad.Eff (Eff)
import Graphics.Canvas (Context2D, CANVAS, CanvasGradient, createLinearGradient, createRadialGradient, addColorStop)
import Color (Color, graytone, toHSLA, toRGBA, rgba, complementary, cssStringHSLA)
import Color (hsla) as Color
import Prelude (class Eq, eq, (==), (&&), ($), (*), (/), discard, (>>=), pure)
import Data.Tuple (Tuple(..))
import Data.Foldable (for_)
import Elm.Basics (Float, degrees)
import Elm.List (List)
import Math (pi)


-- | Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
-- | with an alpha component for transparency.
-- |
-- | Note that the hue is expressed in radians
hsla :: Float -> Float -> Float -> Float -> Color
hsla h = Color.hsla $ h * 180.0 / pi


-- | Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV). This gives
-- | you access to colors more like a color wheel, where all hues are arranged in a
-- | circle that you specify with standard Elm angles (radians).
-- |
-- |     red   = hsl (degrees   0) 1 0.5
-- |     green = hsl (degrees 120) 1 0.5
-- |     blue  = hsl (degrees 240) 1 0.5
-- |     pastelRed = hsl (degrees 0) 0.7 0.7
-- |
-- | To cycle through all colors, just cycle through degrees. The saturation level
-- | is how vibrant the color is, like a dial between grey and bright colors. The
-- | lightness level is a dial between white and black.
-- |
-- | Note that the hue is expressed in radians.
hsl :: Float -> Float -> Float -> Color
hsl h s l = hsla h s l 1.0


-- | Produce a gray based on the input. 0 is white, 1 is black.
grayscale :: Float -> Color
grayscale = graytone


-- | Produce a gray based on the input. 0 is white, 1 is black.
greyscale :: Float -> Color
greyscale = graytone


-- | Produce a &ldquo;complementary color&rdquo;. The two colors will
-- | accent each other. This is the same as rotating the hue by 180&deg;.
complement :: Color -> Color
complement = complementary


-- | Extract the components of a color in the HSL format.
-- |
-- | Note that the hue is expressed in radians.
toHsl :: Color -> { hue :: Float, saturation :: Float, lightness :: Float, alpha :: Float }
toHsl color =
    let
        parts =
            toHSLA color

    in
        { hue: degrees parts.h
        , saturation: parts.s
        , lightness: parts.l
        , alpha: parts.a
        }


-- | Extract the components of a color in the RGB format.
toRgb :: Color -> { red :: Int, green :: Int, blue :: Int, alpha :: Float }
toRgb color =
    let
        parts =
            toRGBA color

    in
        { red: parts.r
        , green: parts.g
        , blue: parts.b
        , alpha: parts.a
        }


-- | A CSS representation of the color in the form hsl(..) or hsla(...).
toCss :: Color -> String
toCss = cssStringHSLA


type Point =
    { x :: Float
    , y :: Float
    }

eqPoint :: Point -> Point -> Boolean
eqPoint p0 p1 = p0.x == p1.x && p0.y == p1.y


-- | Abstract representation of a color gradient.
data Gradient
    = Linear Point Point (List (Tuple Float Color))
    | Radial Point Float Point Float (List (Tuple Float Color))

instance eqGradient :: Eq Gradient where
    eq (Linear a1 b1 c1) (Linear a2 b2 c2) = eqPoint a1 a2 && eqPoint b1 b2 && eq c1 c2
    eq (Radial a1 b1 c1 d1 e1) (Radial a2 b2 c2 d2 e2) = eqPoint a2 a2 && eq b1 b2 && eqPoint c1 c2 && eq d1 d2 && eq e1 e2
    eq _ _ = false


-- | Create a linear gradient. Takes a start and end point and then a series of
-- | &ldquo;color stops&rdquo; that indicate how to interpolate between the start and
-- | end points. See [this example](http://elm-lang.org/examples/linear-gradient) for a
-- | more visual explanation.
linear :: Tuple Float Float -> Tuple Float Float -> List (Tuple Float Color) -> Gradient
linear (Tuple startX startY) (Tuple endX endY) =
    Linear
        { x: startX, y: startY }
        { x: endX,   y: endY   }


-- | Create a radial gradient. First takes a start point and inner radius.  Then
-- | takes an end point and outer radius. It then takes a series of &ldquo;color
-- | stops&rdquo; that indicate how to interpolate between the inner and outer
-- | circles. See [this example](http://elm-lang.org/examples/radial-gradient) for a
-- | more visual explanation.
radial :: Tuple Float Float -> Float -> Tuple Float Float -> Float -> List (Tuple Float Color) -> Gradient
radial (Tuple startX startY) radius (Tuple endX endY) =
    Radial
        { x: startX, y: startY }
        radius
        { x: endX, y: endY }


-- | Make a CanvasGradient from a Gradient.
toCanvasGradient :: ∀ e. Gradient -> Context2D -> Eff (canvas :: CANVAS | e) CanvasGradient
toCanvasGradient grad ctx =
    case grad of
        Linear p0 p1 stops ->
            -- Note that I seem to have to swap the beginning and the end
            createLinearGradient
                { x1: p0.x
                , y1: p0.y
                , x0: p1.x
                , y0: p1.y
                }
                ctx
            >>= addStops stops

        Radial p0 r0 p1 r1 stops ->
            createRadialGradient
                { x0: p0.x
                , y0: p0.y
                , r0
                , x1: p1.x
                , y1: p1.y
                , r1
                }
                ctx
            >>= addStops stops


addStops :: ∀ e. List (Tuple Float Color) -> CanvasGradient -> Eff (canvas :: CANVAS | e) CanvasGradient
addStops list grad = do
    for_ list \(Tuple stop color) ->
        addColorStop stop (toCss color) grad

    pure grad


-- BUILT-IN COLORS

lightRed :: Color
lightRed = rgba 239 41 41 1.0


red :: Color
red = rgba 204 0 0 1.0


darkRed :: Color
darkRed = rgba 164 0 0 1.0


lightOrange :: Color
lightOrange = rgba 252 175 62 1.0


orange :: Color
orange = rgba 245 121 0 1.0


darkOrange :: Color
darkOrange = rgba 206 92 0 1.0


lightYellow :: Color
lightYellow = rgba 255 233 79 1.0


yellow :: Color
yellow = rgba 237 212 0 1.0


darkYellow :: Color
darkYellow = rgba 196 160 0 1.0


lightGreen :: Color
lightGreen = rgba 138 226 52 1.0


green :: Color
green = rgba 115 210 22 1.0


darkGreen :: Color
darkGreen = rgba 78 154 6 1.0


lightBlue :: Color
lightBlue = rgba 114 159 207 1.0


blue :: Color
blue = rgba 52 101 164 1.0


darkBlue :: Color
darkBlue = rgba 32 74 135 1.0


lightPurple :: Color
lightPurple = rgba 173 127 168 1.0


purple :: Color
purple = rgba 117 80 123 1.0


darkPurple :: Color
darkPurple = rgba 92 53 102 1.0


lightBrown :: Color
lightBrown = rgba 233 185 110 1.0


brown :: Color
brown = rgba 193 125 17 1.0


darkBrown :: Color
darkBrown = rgba 143 89 2 1.0


black :: Color
black = rgba 0 0 0 1.0


white :: Color
white = rgba 255 255 255 1.0


lightGrey :: Color
lightGrey = rgba 238 238 236 1.0


grey :: Color
grey = rgba 211 215 207 1.0


darkGrey :: Color
darkGrey = rgba 186 189 182 1.0


lightGray :: Color
lightGray = rgba 238 238 236 1.0


gray :: Color
gray = rgba 211 215 207 1.0


darkGray :: Color
darkGray = rgba 186 189 182 1.0


lightCharcoal :: Color
lightCharcoal = rgba 136 138 133 1.0


charcoal :: Color
charcoal = rgba 85 87 83 1.0


darkCharcoal :: Color
darkCharcoal = rgba 46 52 54 1.0

