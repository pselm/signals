## Module Elm.Transform2D

A library for performing 2D matrix transformations.
It is used primarily with the `groupTransform` function from
`Elm.Graphics.Collage` and allows you to do things
like rotation, scaling, translation, shearing, and reflection.

Note that all the matrices in this library are 3x3 matrices of homogeneous
coordinates, used for [affine transformations](http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
Since the bottom row is always `0 0 1` in these matrices, it is omitted in
the diagrams below.

This is implemented as a wrapper around the purescript-canvas module, and its `Transform` type.

#### `Transform2D`

``` purescript
type Transform2D = Transform
```

A matrix representing a 2D transformation.

Equivalent to Purescript's `Graphics.Canvas.Transform`.

#### `toCSS`

``` purescript
toCSS :: Transform2D -> String
```

#### `identity`

``` purescript
identity :: Transform2D
```

Create an identity transform. Transforming by the identity does
not change anything, but it can come in handy as a default or
base case.

        / 1 0 0 \
        \ 0 1 0 /

#### `matrix`

``` purescript
matrix :: Float -> Float -> Float -> Float -> Float -> Float -> Transform2D
```

Create a transformation matrix. This lets you create transforms
such as scales, shears, reflections, and translations.

    matrix a b c d x y

        / a b x \
        \ c d y /

Note that `x` and `y` are the translation values.

#### `rotation`

``` purescript
rotation :: Float -> Transform2D
```

Create a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
Given an angle t, it creates a counterclockwise rotation matrix:

    rotation t

        / cos t  -sin t  0 \
        \ sin t   cos t  0 /

#### `translation`

``` purescript
translation :: Float -> Float -> Transform2D
```

Create a transformation matrix for translation.

    translation x y

        / 1 0 x \
        \ 0 1 y /

#### `scale`

``` purescript
scale :: Float -> Transform2D
```

Creates a transformation matrix for scaling by all directions.

    scale s

      / s 0 0 \
      \ 0 s 0 /

#### `scaleX`

``` purescript
scaleX :: Float -> Transform2D
```

Create a transformation for horizontal scaling.

#### `scaleY`

``` purescript
scaleY :: Float -> Transform2D
```

Create a transformation for vertical scaling.

#### `multiply`

``` purescript
multiply :: Transform2D -> Transform2D -> Transform2D
```

Multiply two transforms together.

    multiply m n

        / ma mb mx \     / na nb nx \
        | mc md my |  .  | nc nd ny |
        \  0  0  1 /     \  0  0  1 /


