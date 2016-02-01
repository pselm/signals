module Test.Elm.ResultTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Result as Result
import Elm.Result (Result(..))
import Prelude (bind, Eq)
import Elm.Basics ((<|), (==), (+), (%), (++))
import Elm.Maybe (Maybe(..))


assertEqual :: forall e. String -> Result String Int -> Result String Int -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual


assertMaybe :: forall e. String -> Maybe Int -> Maybe Int -> Assertion e
assertMaybe name expected actual =
    assert name <| expected == actual


isEven :: Int -> Result String Int
isEven n =
    if n % 2 == 0
        then Ok n
        else Err "number is odd"


add3 :: Int -> Int -> Int -> Int
add3 a b c =
    a + b + c


add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d =
    a + b + c + d


add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5 a b c d e =
    a + b + c + d + e


tests :: forall e. TestUnit e
tests = test "Elm.Result\n" do
    test "Result.map" do
        assertEqual "map Ok"  (Ok 3)        (Result.map ((+) 1) (Ok 2))
        assertEqual "map Err" (Err "error") (Result.map ((+) 1) (Err "error"))
    
    test "Result.mapN" do
        assertEqual "map2 Ok"  (Ok 3)    (Result.map2 (+) (Ok 1) (Ok 2))
        assertEqual "map2 Err" (Err "x") (Result.map2 (+) (Ok 1) (Err "x"))

        assertEqual "map3 Ok"  (Ok 6)    (Result.map3 add3 (Ok 1) (Ok 2) (Ok 3))
        assertEqual "map3 Err" (Err "x") (Result.map3 add3 (Ok 1) (Ok 2) (Err "x"))

        assertEqual "map4 Ok"  (Ok 10)   (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Ok 4))
        assertEqual "map4 Err" (Err "x") (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Err "x"))

        assertEqual "map5 Ok"  (Ok 15)   (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Ok 5))
        assertEqual "map5 Err" (Err "x") (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Err "x"))
      
    test "Result.andThen" do
        assertEqual "andThen Ok" (Ok 42) ((Ok 42) `Result.andThen` isEven)

        assertEqual "andThen first Err"
            (Err "could not convert string '4.2' to an Int")
            ((Err "could not convert string '4.2' to an Int") `Result.andThen` isEven)
        
        assertEqual "andThen second Err"
            (Err "number is odd")
            ((Ok 41) `Result.andThen` isEven)

    test "Result.withDefault" do
        assert "Ok" <|  Result.withDefault 0 (Ok 123) == 123
        assert "Err" <| Result.withDefault 0 (Err "abc") == 0
    
    test "Result.formatError" do
        assertEqual "Ok"  (Result.formatError (\e -> "Error: " ++ e) (Ok 123)) (Ok 123)
        assertEqual "Err" (Result.formatError (\e -> "Error: " ++ e) (Err "abc")) (Err "Error: abc")
    
    test "Result.toMaybe" do
        assertMaybe "Ok"  (Result.toMaybe (Ok 27)) (Just 27)
        assertMaybe "Err" (Result.toMaybe (Err "bob")) Nothing

    test "Result.fromMaybe" do
        assertEqual "Ok"  (Result.fromMaybe "bad" (Just 27)) (Ok 27)
        assertEqual "Err" (Result.fromMaybe "bad" (Nothing)) (Err "bad")
