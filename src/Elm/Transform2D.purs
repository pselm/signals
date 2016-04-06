-- | A library for performing 2D matrix transformations.
-- | It is used primarily with the `groupTransform` function from
-- | `Elm.Graphics.Collage` and allows you to do things
-- | like rotation, scaling, translation, shearing, and reflection.
-- |
-- | Note that all the matrices in this library are 3x3 matrices of homogeneous
-- | coordinates, used for [affine transformations](http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
-- | Since the bottom row is always `0 0 1` in these matrices, it is omitted in the diagrams below.

module Transform2D
    ( Transform2D
    , identity, matrix, multiply
    , rotation, translation
    , scale, scaleX, scaleY
    ) where


import Data.TypedArray (asFloat32Array, unsafeIndex)
import Data.ArrayBuffer.Types (Float32Array)
import Math (cos, sin)
import Prelude (($), (*), (+), negate)
import Elm.Basics (Float)


-- | A matrix representing a 2D transformation.
newtype Transform2D = Transform2D Float32Array


-- | Create an identity transform. Transforming by the identity does
-- | not change anything, but it can come in handy as a default or
-- | base case.
-- |
-- |         / 1 0 0 \
-- |         \ 0 1 0 /
identity :: Transform2D
identity =
    Transform2D $
        asFloat32Array [1.0, 0.0, 0.0, 0.0, 1.0, 0.0]


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
matrix m11 m12 m21 m22 dx dy =
    Transform2D $
        asFloat32Array [m11, m12, dx, m21, m22, dy]


-- | Create a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
-- | Given an angle t, it creates a counterclockwise rotation matrix:
-- |
-- |     rotation t
-- |
-- |         / cos t  -sin t  0 \
-- |         \ sin t   cos t  0 /
rotation :: Float -> Transform2D
rotation t =
    Transform2D $
        asFloat32Array [c, (-s), 0.0, s, c, 0.0]

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


-- | Creates a transformation matrix for scaling by a all directions.
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


-- | Create a transformation for vertical scaling. -}
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
multiply (Transform2D m) (Transform2D n) =
    Transform2D $
        asFloat32Array
            [ m11 * n11 + m12 * n21
            , m11 * n12 + m12 * n22
            , m11 * ndx + m12 * ndy + mdx
            , m21 * n11 + m22 * n21
            , m21 * n12 + m22 * n22
            , m21 * ndx + m22 * ndy + mdy
            ]

    where
        m11 = unsafeIndex m 0
        m12 = unsafeIndex m 1
        m21 = unsafeIndex m 3
        m22 = unsafeIndex m 4
        mdx = unsafeIndex m 2
        mdy = unsafeIndex m 5

        n11 = unsafeIndex n 0
        n12 = unsafeIndex n 1
        n21 = unsafeIndex n 3
        n22 = unsafeIndex n 4
        ndx = unsafeIndex n 2
        ndy = unsafeIndex n 5
