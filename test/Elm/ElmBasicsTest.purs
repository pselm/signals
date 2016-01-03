module Test.Elm.ElmBasicsTest (tests) where

import Test.Unit
import Test.Unit.Console

import Elm.Basics
import Elm.List (List(..), (:))
import Prelude (bind, Ordering(..), Eq)


assertEqual :: forall a. (Eq a) => String -> a -> a -> Test ( testOutput :: TestOutput )
assertEqual name expected actual =
    assert name <| expected == actual 


tests :: Test ( testOutput :: TestOutput )
tests = do
    test "Comparison" do
        assertEqual "max" 42 (max 32 42)
        assertEqual "min" 42 (min 91 42)
        assertEqual "clamp low" 10 (clamp 10 20 5)
        assertEqual "clamp mid" 15 (clamp 10 20 15)
        assertEqual "clamp high" 20 (clamp 10 20 25)
        assertEqual "5 < 6" true (5 < 6)
        assertEqual "6 < 5" false (6 < 5)
        assertEqual "6 < 6" false (6 < 6)
        assertEqual "5 > 6" false (5 > 6)
        assertEqual "6 > 5" true (6 > 5)
        assertEqual "6 > 6" false (6 > 6)
        assertEqual "5 <= 6" true (5 <= 6)
        assertEqual "6 <= 5" false (6 <= 5)
        assertEqual "6 <= 6" true (6 <= 6)
        assertEqual "compare \"A\" \"B\"" LT (compare "A" "B")
        assertEqual "compare 'f' 'f'" EQ (compare 'f' 'f')
        assertEqual "compare (1, 2, 3, 4, 5, 6) (0, 1, 2, 3, 4, 5)" GT (compare (1 : 2 : 3 : 4 : 5 : 6 : Nil) (0 : 1 : 2 : 3 : 4 : 5 : Nil))
        assertEqual "compare ['a'] ['b']" LT (compare ('a' : Nil) ('b' : Nil))
       
    test "toString Tests" do
        assertEqual "toString Int" "42" (toString 42)
        assertEqual "toString Float" "42.52" (toString 42.52)
        assertEqual "toString Char" "'c'" (toString 'c')
        assertEqual "toString Char single quote" "'\\''" (toString '\'')
        assertEqual "toString Char double quote" "'\"'" (toString '"')
        assertEqual "toString String single quote" "\"not 'escaped'\"" (toString "not 'escaped'")
        assertEqual "toString String double quote" "\"are \\\"escaped\\\"\"" (toString "are \"escaped\"")

    test "Trigonometry Tests" do
        assertEqual "radians 0" 0.0 (radians 0.0)
        assertEqual "radians positive" 5.0 (radians 5.0)
        assertEqual "radians negative" (-5.0) (radians (-5.0))
        assertEqual "degrees 0" 0.0 (degrees 0.0)
        assert "degrees 90" (abs (1.57 - degrees 90.0) < 0.01) -- This should test to enough precision to know if anything's breaking
        assert "degrees -145" (abs ((-2.53) - degrees (-145.0)) < 0.01) -- This should test to enough precision to know if anything's breaking
        assertEqual "turns 0" 0.0 (turns 0.0)
        assert "turns 8" (abs (50.26 - turns 8.0) < 0.01) -- This should test to enough precision to know if anything's breaking
        assert "turns -133" (abs ((-835.66) - turns (-133.0)) < 0.01) -- This should test to enough precision to know if anything's breaking
       
        let fp1 = fromPolar {r: 0.0, theta: 0.0}
        let fp2 = fromPolar {r: 1.0, theta: 0.0}
        let fp3 = fromPolar {r: 0.0, theta: 1.0}
        let fp4 = fromPolar {r: 1.0, theta: 1.0}

        assert "fromPolar (0, 0)" <| fp1.x == 0.0 && fp1.y == 0.0
        assert "fromPolar (1, 0)" <| fp2.x == 1.0 && fp2.y == 0.0
        assert "fromPolar (0, 1)" <| fp3.x == 0.0 && fp3.y == 0.0
        assert "fromPolar (1, 1)" <| 0.54 - fp4.x < 0.01 && 0.84 - fp4.y < 0.01

        let tp1 = toPolar {x: 0.0, y: 0.0}
        let tp2 = toPolar {x: 1.0, y: 0.0}
        let tp3 = toPolar {x: 0.0, y: 1.0}
        let tp4 = toPolar {x: 1.0, y: 1.0}

        assert "toPolar (0, 0)" <| tp1.r == 0.0 && tp1.theta == 0.0
        assert "toPolar (1, 0)" <| tp2.r == 1.0 && tp2.theta == 0.0
        assert "toPolar (0, 1)" <| tp3.r == 1.0 && abs (1.57 - tp3.theta) < 0.01
        assert "toPolar (1, 1)" <| abs (1.41 - tp4.r) < 0.01 && abs (0.78 - tp4.theta) < 0.01

        assertEqual "cos" 1.0 (cos 0.0)
        assertEqual "sin" 0.0 (sin 0.0)
        assert "tan" (abs (12.67 - tan 17.2) < 0.01)
        assert "acos" (abs (3.14 - acos (-1.0)) < 0.01)
        assert "asin" (abs (0.30 - asin 0.3) < 0.01)
        assert "atan" (abs (1.57 - atan 4567.8) < 0.01)
        assert "atan2" (abs (1.55 - atan2 36.0 0.65) < 0.01)
        assert "pi" (abs (3.14 - pi) < 0.01)
    
    test "Basic Math Tests" do
        assertEqual "add float" 159.0 (155.6 + 3.4)
        assertEqual "add int" 17 (10 + 7)
        assertEqual "subtract float" (-6.3) (1.0 - 7.3)
        assertEqual "subtract int" 1130 (9432 - 8302)
        assertEqual "multiply float" 432.0 (96.0 * 4.5)
        assertEqual "multiply int" 90 (10 * 9)
        assertEqual "divide float" 13.175 (527.0 / 40.0)
        assertEqual "divide int" 23 (70 // 3)
        assertEqual "7 `rem` 2" 1 (7 `rem` 2)
        assertEqual "-1 `rem` 4" (-1) ((-1) `rem` 4)
        assertEqual "7 % 2" 1 (7 % 2)
        assertEqual "-1 % 4" 3 ((-1) % 4)
        assertEqual "3^2" 9 (3^2)
        assertEqual "sqrt" 9.0 (sqrt 81.0)
        assertEqual "negate 42" (-42) (negate 42)
        assertEqual "negate -42" 42 (negate (-42))
        assertEqual "negate 0" 0 (negate 0)
        assertEqual "abs -25" 25 (abs (-25))
        assertEqual "abs 76" 76 (abs 76)
        assertEqual "logBase 10 100" 2.0 (logBase 10.0 100.0)
        assertEqual "logBase 2 256" 8.0 (logBase 2.0 256.0)
        assert "e" <| abs (2.72 - e) < 0.01

    test "Boolean Tests" do
        assertEqual "false && false" false (false && false)
        assertEqual "false && true" false (false && true)
        assertEqual "true && false" false (true && false)
        assertEqual "true && true" true (true && true)
        assertEqual "false || false" false (false || false)
        assertEqual "false || true" true (false || true)
        assertEqual "true || false" true (true || false)
        assertEqual "true || true" true (true || true)
        assertEqual "xor false false" false (xor false false)
        assertEqual "xor false true" true (xor false true)
        assertEqual "xor true false" true (xor true false)
        assertEqual "xor true true" false (xor true true)
        assertEqual "not true" false (not true)
        assertEqual "not false" true (not false)
        
    test "Conversion Tests" do
        assertEqual "round 0.6" 1 (round 0.6)
        assertEqual "round 0.4" 0 (round 0.4)
        assertEqual "round 0.5" 1 (round 0.5)
        assertEqual "truncate -2367.9267" (-2367) (truncate (-2367.9267))
        assertEqual "floor -2367.9267" (-2368) (floor (-2367.9267))
        assertEqual "ceiling 37.2" 38 (ceiling 37.2)
        assertEqual "toFloat 25" 25.0 (toFloat 25)
        
    
    test "Miscellaneous Tests" do
        assertEqual "isNaN (0/0)" true (isNaN (0.0 / 0.0))
        assertEqual "isNaN (sqrt -1)" true (isNaN (sqrt (-1.0)))
        assertEqual "isNaN (1/0)" false (isNaN (1.0 / 0.0))
        assertEqual "isNaN 1" false (isNaN 1.0)
        assertEqual "isInfinite (0/0)" false (isInfinite (0.0 / 0.0))
        assertEqual "isInfinite (sqrt -1)" false (isInfinite (sqrt (-1.0)))
        assertEqual "isInfinite (1/0)" true (isInfinite (1.0 / 0.0))
        assertEqual "isInfinite 1" false (isInfinite 1.0)
        assertEqual "\"hello\" ++ \"world\"" "helloworld" ("hello" ++ "world")
        assertEqual "[1, 1, 2] ++ [3, 5, 8]" (1 : 1 : 2 : 3 : 5 : 8 : Nil) ((1 : 1 : 2 : Nil) ++ (3 : 5 : 8 : Nil))
        assertEqual "fst (1, 2)" 1 (fst (Tuple 1 2))
        assertEqual "snd (1, 2)" 2 (snd (Tuple 1 2))        
    
    test "Higher Order Helpers Tests" do
        assertEqual "identity 'c'" 'c' (identity 'c')
        assertEqual "always 42 ()" 42 (always 42 "bob")
        assertEqual "<|" 9 (identity <| 3 + 6)
        assertEqual "|>" 9 (3 + 6 |> identity)
        assertEqual "<<" true (not << xor true <| true)
        assertEqual ">>" true (true |> xor true >> not)
        assertEqual "flip" 10 ((flip (//)) 2 20)
        assertEqual "curry" 1 ((curry (\t -> (fst t) + (snd t))) (-5) 6)
        assertEqual "uncurry" 1 ((uncurry (+)) (Tuple (-5) 6)) 
