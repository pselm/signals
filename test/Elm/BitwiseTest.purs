module Test.Elm.BitwiseTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Bitwise as Bitwise
import Prelude (bind, Eq)
import Elm.Basics ((<|), (==), negate)


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e 
assertEqual name expected actual =
    assert name <| expected == actual 


tests :: forall e. TestUnit e
tests = do
    test "Bitwise.and" do
        assertEqual "and with 32 bit integers" 1 (Bitwise.and 5 3)
        assertEqual "and with 0 as first argument" 0 (Bitwise.and 0 1450)
        assertEqual "and with 0 as second argument" 0 (Bitwise.and 274 0)
        assertEqual "and with -1 as first argument" 2671 (Bitwise.and (-1) 2671)
        assertEqual "and with -1 as second argument" 96 (Bitwise.and 96 (-1))

    test "Bitwise.or" do
        assertEqual "or with 32 bit integers" 15 (Bitwise.or 9 14)
        assertEqual "or with 0 as first argument" 843 (Bitwise.or 0 843)
        assertEqual "or with 0 as second argument" 19 (Bitwise.or 19 0)
        assertEqual "or with -1 as first argument" (-1) (Bitwise.or (-1) 2360)
        assertEqual "or with -1 as second argument" (-1) (Bitwise.or 3 (-1))
        
    test "Bitwise.xor" do
        assertEqual "xor with 32 bit integers" 604 (Bitwise.xor 580 24)
        assertEqual "xor with 0 as first argument" 56 (Bitwise.xor 0 56)
        assertEqual "xor with 0 as second argument" (-268) (Bitwise.xor (-268) 0)
        assertEqual "xor with -1 as first argument" (-25) (Bitwise.xor (-1) 24)
        assertEqual "xor with -1 as second argument" 25601 (Bitwise.xor (-25602) (-1))
        
    test "Bitwise.complement" do
        assertEqual "complement a positive" (-9) (Bitwise.complement 8)
        assertEqual "complement a negative" 278 (Bitwise.complement (-279))
        
    test "Bitwise.shiftLeft" do
        assertEqual "8 `shiftLeft` 1 == 16" 16 (Bitwise.shiftLeft 8 1)
        assertEqual "8 `shiftLeft` 2 == 32" 32 (Bitwise.shiftLeft 8 2)

    test "Bitwise.shiftRight" do
        assertEqual "32 `shiftRight` 1 == 16" 16 (Bitwise.shiftRight 32 1)
        assertEqual "32 `shiftRight` 2 == 8" 8 (Bitwise.shiftRight 32 2)
        assertEqual "-32 `shiftRight` 1 == -16" (-16) (Bitwise.shiftRight (-32) 1)

    test "Bitwise.shiftRightLogical" do
        assertEqual "32 `shiftRightLogical` 1 == 16" 16 (Bitwise.shiftRightLogical 32 1)
        assertEqual "32 `shiftRightLogical` 2 == 8" 8 (Bitwise.shiftRightLogical 32 2)
        assertEqual "-32 `shiftRightLogical` 1 == 2147483632" 2147483632 (Bitwise.shiftRightLogical (-32) 1)

