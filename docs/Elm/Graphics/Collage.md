## Module Elm.Graphics.Collage

The collage API is for freeform graphics. You can move, rotate, scale, etc.
all sorts of forms including lines, shapes, images, and elements.

Collages use the same coordinate system you might see in an algebra or physics
problem. The origin (0,0) is at the center of the collage, not the top left
corner as in some other graphics libraries. Furthermore, the y-axis points up,
so moving a form 10 units in the y-axis will move it up on screen.

#### `Form`

``` purescript
newtype Form
```

A visual `Form` has a shape and texture. This can be anything from a red
square to a circle textured with stripes.

#### `LineCap`

``` purescript
data LineCap
  = Flat
  | Round
  | Padded
```

The shape of the ends of a line.

##### Instances
``` purescript
Eq LineCap
```

#### `LineJoin`

``` purescript
data LineJoin
  = Smooth
  | Sharp Float
  | Clipped
```

The shape of the &ldquo;joints&rdquo; of a line, where each line segment
meets. `Sharp` takes an argument to limit the length of the joint. This
defaults to 10.

##### Instances
``` purescript
Eq LineJoin
```

#### `LineStyle`

``` purescript
type LineStyle = { color :: Color, width :: Float, cap :: LineCap, join :: LineJoin, dashing :: List Int, dashOffset :: Int }
```

All of the attributes of a line style. This lets you build up a line style
however you want. You can also update existing line styles with record updates.

#### `defaultLine`

``` purescript
defaultLine :: LineStyle
```

The default line style, which is solid black with flat caps and sharp joints.
You can use record updates to build the line style you
want. For example, to make a thicker line, you could say:

    defaultLine { width = 10 }

#### `solid`

``` purescript
solid :: Color -> LineStyle
```

Create a solid line style with a given color.

#### `dashed`

``` purescript
dashed :: Color -> LineStyle
```

Create a dashed line style with a given color. Dashing equals `[8,4]`.

#### `dotted`

``` purescript
dotted :: Color -> LineStyle
```

Create a dotted line style with a given color. Dashing equals `[3,3]`.

#### `filled`

``` purescript
filled :: Color -> Shape -> Form
```

Create a filled in shape.

#### `textured`

``` purescript
textured :: String -> Shape -> Form
```

Create a textured shape. The texture is described by some url and is
tiled to fill the entire shape.

#### `gradient`

``` purescript
gradient :: Gradient -> Shape -> Form
```

Fill a shape with a gradient.

#### `outlined`

``` purescript
outlined :: LineStyle -> Shape -> Form
```

Outline a shape with a given line style.

#### `traced`

``` purescript
traced :: LineStyle -> Path -> Form
```

Trace a path with a given line style.

#### `toForm`

``` purescript
toForm :: forall a. Renderable a => a -> Form
```

Turn any `Element` into a `Form`. This lets you use text, gifs, and video
in your collage. This means you can move, rotate, and scale
an `Element` however you want.

In fact, this works with any `Renderable`, not just Elements.

#### `group`

``` purescript
group :: List Form -> Form
```

Flatten many forms into a single `Form`. This lets you move and rotate them
as a single unit, making it possible to build small, modular components.
Forms will be drawn in the order that they are listed, as in `collage`.

#### `groupTransform`

``` purescript
groupTransform :: Transform2D -> List Form -> Form
```

Flatten many forms into a single `Form` and then apply a matrix
transformation. Forms will be drawn in the order that they are listed, as in
`collage`.

#### `move`

``` purescript
move :: Point -> Form -> Form
```

Move a form by the given amount (x, y). This is a relative translation so
`(move (5,10) form)` would move `form` five pixels to the right and ten pixels up.

#### `moveX`

``` purescript
moveX :: Float -> Form -> Form
```

Move a shape in the x direction. This is relative so `(moveX 10 form)` moves
`form` 10 pixels to the right.

#### `moveY`

``` purescript
moveY :: Float -> Form -> Form
```

Move a shape in the y direction. This is relative so `(moveY 10 form)` moves
`form` upwards by 10 pixels.

#### `scale`

``` purescript
scale :: Float -> Form -> Form
```

Scale a form by a given factor. Scaling by 2 doubles both dimensions,
and quadruples the area.

#### `rotate`

``` purescript
rotate :: Float -> Form -> Form
```

Rotate a form by a given angle. Rotate takes standard Elm angles (radians)
and turns things counterclockwise. So to turn `form` 30&deg; to the left
you would say, `(rotate (degrees 30) form)`.

#### `alpha`

``` purescript
alpha :: Float -> Form -> Form
```

Set the alpha of a `Form`. The default is 1, and 0 is totally transparent.

#### `Collage`

``` purescript
newtype Collage
```

##### Instances
``` purescript
Renderable Collage
```

#### `makeCollage`

``` purescript
makeCollage :: Int -> Int -> List Form -> Collage
```

Create a `Collage` with certain dimensions and content. It takes width and height
arguments to specify dimensions, and then a list of 2D forms to decribe the content.

The forms are drawn in the order of the list, i.e., `collage w h (a : b : Nil)` will
draw `b` on top of `a`.

Note that this normally might be called `collage`, but Elm uses that for the function
that actually creates an `Element`.

#### `collage`

``` purescript
collage :: Int -> Int -> List Form -> Element
```

Create a collage `Element` with certain dimensions and content. It takes width and height
arguments to specify dimensions, and then a list of 2D forms to decribe the content.

The forms are drawn in the order of the list, i.e., `collage w h (a : b : Nil)` will
draw `b` on top of `a`.

To make a `Collage` without immediately turning it into an `Element`, see `makeCollage`.

#### `toElement`

``` purescript
toElement :: Collage -> Element
```

Turn a `Collage` into an `Element`.

#### `Path`

``` purescript
newtype Path
```

A 2D path. Paths are a sequence of points. They do not have a color.

#### `path`

``` purescript
path :: List Point -> Path
```

Create a path that follows a sequence of points.

#### `segment`

``` purescript
segment :: Point -> Point -> Path
```

Create a path along a given line segment.

#### `Shape`

``` purescript
newtype Shape
```

A 2D shape. Shapes are closed polygons. They do not have a color or
texture, that information can be filled in later.

#### `polygon`

``` purescript
polygon :: List Point -> Shape
```

Create an arbitrary polygon by specifying its corners in order.
`polygon` will automatically close all shapes, so the given list
of points does not need to start and end with the same position.

#### `rect`

``` purescript
rect :: Float -> Float -> Shape
```

A rectangle with a given width and height, centered on the origin.

#### `square`

``` purescript
square :: Float -> Shape
```

A square with a given edge length, centred on the origin.

#### `oval`

``` purescript
oval :: Float -> Float -> Shape
```

An oval with a given width and height.

#### `circle`

``` purescript
circle :: Float -> Shape
```

A circle with a given radius.

#### `ngon`

``` purescript
ngon :: Int -> Float -> Shape
```

A regular polygon with N sides. The first argument specifies the number
of sides and the second is the radius. So to create a pentagon with radius
30 you would say:

    ngon 5 30

#### `text`

``` purescript
text :: Text -> Form
```

Create some text. Details like size and color are part of the `Text` value
itself, so you can mix colors and sizes and fonts easily.

#### `outlinedText`

``` purescript
outlinedText :: LineStyle -> Text -> Form
```

Create some outlined text. Since we are just outlining the text, the color
is taken from the `LineStyle` attribute instead of the `Text`.


