## Module Elm.Color

Library for working with colors. Includes
[RGB](https://en.wikipedia.org/wiki/RGB_color_model) and
[HSL](http://en.wikipedia.org/wiki/HSL_and_HSV) creation, gradients, and
built-in names.

The Purescript implementation is built on top of the purescript-colors module
and its `Color` type. So, you can also use functions from that module.

#### `hsla`

``` purescript
hsla :: Float -> Float -> Float -> Float -> Color
```

Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
with an alpha component for transparency.

Note that the hue is expressed in radians

#### `hsl`

``` purescript
hsl :: Float -> Float -> Float -> Color
```

Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV). This gives
you access to colors more like a color wheel, where all hues are arranged in a
circle that you specify with standard Elm angles (radians).

    red   = hsl (degrees   0) 1 0.5
    green = hsl (degrees 120) 1 0.5
    blue  = hsl (degrees 240) 1 0.5
    pastelRed = hsl (degrees 0) 0.7 0.7

To cycle through all colors, just cycle through degrees. The saturation level
is how vibrant the color is, like a dial between grey and bright colors. The
lightness level is a dial between white and black.

Note that the hue is expressed in radians.

#### `grayscale`

``` purescript
grayscale :: Float -> Color
```

Produce a gray based on the input. 0 is white, 1 is black.

#### `greyscale`

``` purescript
greyscale :: Float -> Color
```

Produce a gray based on the input. 0 is white, 1 is black.

#### `complement`

``` purescript
complement :: Color -> Color
```

Produce a &ldquo;complementary color&rdquo;. The two colors will
accent each other. This is the same as rotating the hue by 180&deg;.

#### `toHsl`

``` purescript
toHsl :: Color -> { hue :: Float, saturation :: Float, lightness :: Float, alpha :: Float }
```

Extract the components of a color in the HSL format.

Note that the hue is expressed in radians.

#### `toRgb`

``` purescript
toRgb :: Color -> { red :: Int, green :: Int, blue :: Int, alpha :: Float }
```

Extract the components of a color in the RGB format.

#### `toCss`

``` purescript
toCss :: Color -> String
```

A CSS representation of the color in the form hsl(..) or hsla(...).

#### `Gradient`

``` purescript
data Gradient
```

Abstract representation of a color gradient.

##### Instances
``` purescript
Eq Gradient
```

#### `linear`

``` purescript
linear :: Tuple Float Float -> Tuple Float Float -> List (Tuple Float Color) -> Gradient
```

Create a linear gradient. Takes a start and end point and then a series of
&ldquo;color stops&rdquo; that indicate how to interpolate between the start and
end points. See [this example](http://elm-lang.org/examples/linear-gradient) for a
more visual explanation.

#### `radial`

``` purescript
radial :: Tuple Float Float -> Float -> Tuple Float Float -> Float -> List (Tuple Float Color) -> Gradient
```

Create a radial gradient. First takes a start point and inner radius.  Then
takes an end point and outer radius. It then takes a series of &ldquo;color
stops&rdquo; that indicate how to interpolate between the inner and outer
circles. See [this example](http://elm-lang.org/examples/radial-gradient) for a
more visual explanation.

#### `toCanvasGradient`

``` purescript
toCanvasGradient :: forall e. Gradient -> Context2D -> Eff (canvas :: CANVAS | e) CanvasGradient
```

Make a CanvasGradient from a Gradient.

#### `lightRed`

``` purescript
lightRed :: Color
```

#### `red`

``` purescript
red :: Color
```

#### `darkRed`

``` purescript
darkRed :: Color
```

#### `lightOrange`

``` purescript
lightOrange :: Color
```

#### `orange`

``` purescript
orange :: Color
```

#### `darkOrange`

``` purescript
darkOrange :: Color
```

#### `lightYellow`

``` purescript
lightYellow :: Color
```

#### `yellow`

``` purescript
yellow :: Color
```

#### `darkYellow`

``` purescript
darkYellow :: Color
```

#### `lightGreen`

``` purescript
lightGreen :: Color
```

#### `green`

``` purescript
green :: Color
```

#### `darkGreen`

``` purescript
darkGreen :: Color
```

#### `lightBlue`

``` purescript
lightBlue :: Color
```

#### `blue`

``` purescript
blue :: Color
```

#### `darkBlue`

``` purescript
darkBlue :: Color
```

#### `lightPurple`

``` purescript
lightPurple :: Color
```

#### `purple`

``` purescript
purple :: Color
```

#### `darkPurple`

``` purescript
darkPurple :: Color
```

#### `lightBrown`

``` purescript
lightBrown :: Color
```

#### `brown`

``` purescript
brown :: Color
```

#### `darkBrown`

``` purescript
darkBrown :: Color
```

#### `black`

``` purescript
black :: Color
```

#### `white`

``` purescript
white :: Color
```

#### `lightGrey`

``` purescript
lightGrey :: Color
```

#### `grey`

``` purescript
grey :: Color
```

#### `darkGrey`

``` purescript
darkGrey :: Color
```

#### `lightGray`

``` purescript
lightGray :: Color
```

#### `gray`

``` purescript
gray :: Color
```

#### `darkGray`

``` purescript
darkGray :: Color
```

#### `lightCharcoal`

``` purescript
lightCharcoal :: Color
```

#### `charcoal`

``` purescript
charcoal :: Color
```

#### `darkCharcoal`

``` purescript
darkCharcoal :: Color
```


### Re-exported from Color:

#### `Color`

``` purescript
data Color
```

The representation of a color.

Note:
- The `Eq` instance compares two `Color`s by comparing their (integer) RGB
  values. This is different from comparing the HSL values (for example,
  HSL has many different representations of black (arbitrary hue and
  saturation values).
- Colors outside the sRGB gamut which cannot be displayed on a typical
  computer screen can not be represented by `Color`.


##### Instances
``` purescript
Show Color
Eq Color
```

#### `rgba`

``` purescript
rgba :: Int -> Int -> Int -> Number -> Color
```

Create a `Color` from integer RGB values between 0 and 255 and a floating
point alpha value between 0.0 and 1.0.

#### `rgb`

``` purescript
rgb :: Int -> Int -> Int -> Color
```

Create a `Color` from integer RGB values between 0 and 255.

