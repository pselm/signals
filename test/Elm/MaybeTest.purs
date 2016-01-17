module Test.Elm.MaybeTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Maybe
import Elm.Basics ((+), (<|), (==), sqrt)
import Elm.List (List(..), (:))
import Prelude (bind)


tests :: forall e. TestUnit e
tests = do
    test "Maybe.withDefault" do
        assert "with Just" <| withDefault 100 (Just 42) == 42
        assert "with Nothing" <| withDefault 100 Nothing == 100

    test "Maybe.map" do
        assert "with Just" <| map sqrt (Just 9.0) == Just 3.0
        assert "with Nothing" <| map sqrt Nothing == Nothing

    test "Maybe.map2" do
        assert "both Just" <| map2 (+) (Just 3) (Just 4) == Just 7
        assert "second Nothing" <| map2 (+) (Just 3) Nothing == Nothing
        assert "first Nothing" <| map2 (+) Nothing (Just 4) == Nothing

    test "Maybe.map3" do
        assert "all Just" <| map3 func3 (Just 3) (Just 4) (Just 5) == Just 12
        assert "first Nothing" <| map3 func3 Nothing (Just 4) (Just 5) == Nothing
        assert "second Nothing" <| map3 func3 (Just 3) Nothing (Just 5) == Nothing
        assert "third Nothing" <| map3 func3 (Just 3) (Just 4) Nothing == Nothing

    test "Maybe.map4" do
        assert "all Just" <| map4 func4 (Just 3) (Just 4) (Just 5) (Just 6) == Just 18
        assert "first Nothing" <| map4 func4 Nothing (Just 4) (Just 5) (Just 6) == Nothing
        assert "second Nothing" <| map4 func4 (Just 3) Nothing (Just 5) (Just 6) == Nothing
        assert "third Nothing" <| map4 func4 (Just 3) (Just 4) Nothing (Just 6) == Nothing
        assert "fourth Nothing" <| map4 func4 (Just 3) (Just 4) (Just 5) Nothing == Nothing
    
    test "Maybe.map5" do
        assert "all Just" <| map5 func5 (Just 3) (Just 4) (Just 5) (Just 6) (Just 7) == Just 25
        assert "first Nothing" <| map5 func5 Nothing (Just 4) (Just 5) (Just 6) (Just 7) == Nothing
        assert "second Nothing" <| map5 func5 (Just 3) Nothing (Just 5) (Just 6) (Just 7) == Nothing
        assert "third Nothing" <| map5 func5 (Just 3) (Just 4) Nothing (Just 6) (Just 7) == Nothing
        assert "fourth Nothing" <| map5 func5 (Just 3) (Just 4) (Just 5) Nothing (Just 7) == Nothing
        assert "fifth Nothing" <| map5 func5 (Just 3) (Just 4) (Just 5)  (Just 7)Nothing == Nothing

    test "Maybe.oneOf" do
        assert "second" <| oneOf ( Nothing : Just 42 : Just 71 : Nil ) == Just 42
        assert "third" <| oneOf ( Nothing : Nothing : Just 71 : Nil ) == Just 71
        assert "none" <| oneOf ( Nothing :: Maybe Int : Nothing : Nothing : Nil ) == (Nothing :: Maybe Int)
        assert "nil" <| oneOf (Nil :: List (Maybe Int)) == (Nothing :: Maybe Int)

    test "Maybe.andThen" do
        assert "just just" <| (Just 42) `andThen` (\x -> Just <| x + 1) == Just 43
        assert "nothing just" <| Nothing `andThen` (\x -> Just <| x + 1) == Nothing :: Maybe Int
        assert "just nothing" <| (Just 42) `andThen` (\x -> Nothing) == Nothing :: Maybe Int
        assert "nothing nothing" <| Nothing `andThen` (\x -> Nothing) == Nothing :: Maybe Int

    where
        func3 a b c =
            a + b + c

        func4 a b c d =
            a + b + c + d

        func5 a b c d e =
            a + b + c + d + e
