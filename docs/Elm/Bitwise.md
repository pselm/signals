## Module Elm.Bitwise

Library for [bitwise operations](http://en.wikipedia.org/wiki/Bitwise_operation)

#### `and`

``` purescript
and :: Int -> Int -> Int
```

Bitwise AND

#### `or`

``` purescript
or :: Int -> Int -> Int
```

Bitwise OR

#### `xor`

``` purescript
xor :: Int -> Int -> Int
```

Bitwise XOR

#### `shiftLeft`

``` purescript
shiftLeft :: Int -> Int -> Int
```

Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

    8 `shiftLeft` 1 == 16
    8 `shiftLeft` 2 == 32

#### `shiftRight`

``` purescript
shiftRight :: Int -> Int -> Int
```

Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

     32 `shiftRight` 1 == 16
     32 `shiftRight` 2 == 8
    -32 `shiftRight` 1 == -16

This is called an
[arithmetic right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift),
often written (>>), and sometimes called a sign-propagating
right shift because it fills empty spots with copies of the highest bit.

#### `shiftRightLogical`

``` purescript
shiftRightLogical :: Int -> Int -> Int
```

Shift bits to the right by a given offset, filling new bits with
zeros.

     32 `shiftRightLogical` 1 == 16
     32 `shiftRightLogical` 2 == 8
    -32 `shiftRightLogical` 1 == 2147483632

This is called an
[logical right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift),
often written (>>>), and sometimes called a zero-fill right shift because
it fills empty spots with zeros.


### Re-exported from Data.Int.Bits:

#### `complement`

``` purescript
complement :: Int -> Int
```

Bitwise NOT.

