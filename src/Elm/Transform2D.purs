-- | A library for performing 2D matrix transformations.
-- | It is used primarily with the `groupTransform` function from
-- | `Elm.Graphics.Collage` and allows you to do things
-- | like rotation, scaling, translation, shearing, and reflection.
-- |
-- | Note that all the matrices in this library are 3x3 matrices of homogeneous
-- | coordinates, used for [affine transformations](http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
-- | Since the bottom row is always `0 0 1` in these matrices, it is omitted in
-- | the diagrams below.
-- |
-- | This is implemented as a wrapper around the purescript-canvas module, and its `Transform` type.

module Elm.Transform2D
    ( Transform2D
    , identity, matrix, multiply
    , rotation, translation
    , scale, scaleX, scaleY
    , toCSS
    ) where


import Graphics.Canvas (Transform)
import Data.String (joinWith)
import Math (cos, sin)
import Prelude ((*), (+), negate, (<), (>), (<>), (&&), show)
import Elm.Basics (Float)


-- | A matrix representing a 2D transformation.
-- |
-- | Equivalent to Purescript's `Graphics.Canvas.Transform`.
type Transform2D = Transform


toCSS :: Transform2D -> String
toCSS t =
    "matrix(" <>
    ( joinWith ", "
        [ str t.m11
        , str t.m21
        , str (t.m12)
        , str (-t.m22)
        , str t.m31
        , str t.m32
        ]
    ) <>
    ")"


str :: Number -> String
str n =
    if n < 0.00001 && n > (-0.00001)
        then "0"
        else show n


-- | Create an identity transform. Transforming by the identity does
-- | not change anything, but it can come in handy as a default or
-- | base case.
-- |
-- |         / 1 0 0 \
-- |         \ 0 1 0 /
identity :: Transform2D
identity = translation 0.0 0.0


-- | Create a transformation matrix. This lets you create transforms
-- | such as scales, shears, reflections, and translations.
-- |
-- |     matrix a b c d x y
-- |
-- |         / a b x \
-- |         \ c d y /
-- |
-- | Note that `x` and `y` are the translation values.
matrix :: Float -> Float -> Float -> Float -> Float -> Float -> Transform2D
matrix a b c d x y =
    -- Basically, we translate from the order Elm expects to the labels
    -- that Graphics.Canvas uses. In Graphics.Canvas, the first number
    -- is the column, and the second number the row.
    { m11: a
    , m12: c
    , m21: b
    , m22: d
    , m31: x
    , m32: y
    }


-- | Create a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
-- | Given an angle t, it creates a counterclockwise rotation matrix:
-- |
-- |     rotation t
-- |
-- |         / cos t  -sin t  0 \
-- |         \ sin t   cos t  0 /
rotation :: Float -> Transform2D
rotation t =
    matrix c (-s) s c 0.0 0.0

    where
        c = cos t
        s = sin t


-- | Create a transformation matrix for translation.
-- |
-- |     translation x y
-- |
-- |         / 1 0 x \
-- |         \ 0 1 y /
translation :: Float -> Float -> Transform2D
translation = matrix 1.0 0.0 0.0 1.0


-- | Creates a transformation matrix for scaling by all directions.
-- |
-- |     scale s
-- |
-- |       / s 0 0 \
-- |       \ 0 s 0 /
scale :: Float -> Transform2D
scale s = matrix s 0.0 0.0 s 0.0 0.0


-- | Create a transformation for horizontal scaling.
scaleX :: Float -> Transform2D
scaleX x = matrix x 0.0 0.0 1.0 0.0 0.0


-- | Create a transformation for vertical scaling.
scaleY :: Float -> Transform2D
scaleY y = matrix 1.0 0.0 0.0 y 0.0 0.0


-- | Multiply two transforms together.
-- |
-- |     multiply m n
-- |
-- |         / ma mb mx \     / na nb nx \
-- |         | mc md my |  .  | nc nd ny |
-- |         \  0  0  1 /     \  0  0  1 /
multiply :: Transform2D -> Transform2D -> Transform2D
multiply a b =
    -- This is very translated from Elm's representation to that
    -- of Graphics.Canvas ... will need to test!
    { m11: (a.m11 * b.m11) + (a.m21 * b.m12)
    , m12: (a.m12 * b.m11) + (a.m22 * b.m21)
    , m21: (a.m11 * b.m21) + (a.m21 * b.m22)
    , m22: (a.m12 * b.m21) + (a.m22 * b.m22)
    , m31: (a.m11 * b.m31) + (a.m21 * b.m32) + a.m31
    , m32: (a.m12 * b.m31) + (a.m22 * b.m32) + a.m32
    }
