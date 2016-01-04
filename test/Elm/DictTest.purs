module Test.Elm.DictTest (tests) where

import Test.Unit
import Test.Unit.Console

import Elm.Dict as Dict
import Elm.Dict (Dict())
import Elm.Basics
import Prelude (bind, Eq, (<$>))
import Data.Tuple (Tuple(..))
import Elm.List (List(..), (:))
import Elm.Maybe (Maybe(..))


infixl 9 :=

(:=) :: forall a b. a -> b -> Tuple a b
(:=) = Tuple


assertEqual :: forall a. (Eq a) => String -> a -> a -> Test ( testOutput :: TestOutput )
assertEqual name expected actual =
    assert name <| expected == actual 


animals :: Dict String String
animals =
    Dict.fromList
        ( "Tom" := "cat"
        : "Jerry" := "mouse"
        : Nil
        )

animalsPlus :: Dict String String
animalsPlus =
    Dict.fromList
        ( "Tom" := "Tom: cat"
        : "Jerry" := "Jerry: mouse"
        : Nil
        )


tests :: Test ( testOutput :: TestOutput )
tests = do
    test "Dict.build Tests" do
        assertEqual "empty" (Dict.fromList (Nil :: List (Tuple String String))) (Dict.empty)
        assertEqual "singleton" (Dict.fromList ("k" := "v" : Nil)) (Dict.singleton "k" "v")
        assertEqual "insert" (Dict.fromList ("k" := "v" : Nil)) (Dict.insert "k" "v" Dict.empty)
        assertEqual "insert replace" (Dict.fromList ("k" := "vv" : Nil)) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
        assertEqual "update replace" (Dict.fromList ("k" := "vv" : Nil)) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
        assertEqual "update modify" (Dict.fromList ("k" := "vv" : Nil)) (Dict.update "k" ((<$>) ((++) "v")) (Dict.singleton "k" "v"))
        assertEqual "update new" (Dict.fromList ("k" := "v" : "y" := "vv" : Nil)) (Dict.update "y" (\v -> Just "vv") (Dict.singleton "k" "v"))
        assertEqual "update remove" Dict.empty (Dict.update "k" (\v -> Nothing) (Dict.singleton "k" "v"))
        assertEqual "remove" Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
        assertEqual "remove not found" (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
    
    test "Dict.query Tests" do
        assertEqual "member 1" true (Dict.member "Tom" animals)
        assertEqual "member 2" false (Dict.member "Spike" animals)
        assertEqual "get 1" (Just "cat") (Dict.get "Tom" animals)
        assertEqual "get 2" (Nothing :: Maybe String) (Dict.get "Spike" animals)
        assertEqual "size of empty dictionary" 0 (Dict.size Dict.empty)
        assertEqual "size of example dictionary" 2 (Dict.size animals)
        assertEqual "isEmpty true" true (Dict.isEmpty Dict.empty)
        assertEqual "isEmpty false" false (Dict.isEmpty animals)
        
    test "Dict.combine Tests" do
        assertEqual "union" animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
        assertEqual "union collison" (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
        assertEqual "intersect" (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
        assertEqual "diff" (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
        
    test "Dict.transform Tests" do
        assertEqual "filter" (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        
        let partitioned = Dict.partition (\k v -> k == "Tom") animals
        assertEqual "partition trues" (Dict.singleton "Tom" "cat") partitioned.trues
        assertEqual "partition falses" (Dict.singleton "Jerry" "mouse") partitioned.falses
        
        assertEqual "keys" ("Jerry" : "Tom" : Nil) (Dict.keys animals)
        assertEqual "values" ("mouse" : "cat" : Nil) (Dict.values animals)
        
        assertEqual "dict.map" animalsPlus <|
            Dict.map (\k v -> k ++ ": " ++ v) animals
        
        assertEqual "dict.foldl" ("Tom: cat" : "Jerry: mouse" : Nil) <|
            Dict.foldl (\k v b -> (k ++ ": " ++ v) : b) Nil animals
        
        assertEqual "dict.foldr" ("Jerry: mouse" : "Tom: cat" : Nil) <|
            Dict.foldr (\k v b -> (k ++ ": " ++ v) : b) Nil animals
