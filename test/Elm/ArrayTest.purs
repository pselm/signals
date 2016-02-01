module Test.Elm.ArrayTest (tests) where

import Test.Unit
import Test.Unit.Assert

import qualified Elm.Array as Array
import qualified Elm.List as List
import Prelude (bind, Eq, (-), (==), (*), (+), negate, const)
import Data.List (List(..), toList, (..), (:))
import Data.Maybe (Maybe(..))
import Elm.Basics ((<|), identity, sqrt, (%), always)
import Data.Tuple (Tuple(..))


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual 


mergeSplit :: forall a. Int -> Array.Array a -> Array.Array a
mergeSplit n arr =
    let
        left =
            Array.slice 0 n arr
        
        right =
            Array.slice n (Array.length arr) arr
    
    in
        Array.append left right


holeArray :: Array.Array Int
holeArray =
    List.foldl mergeSplit (Array.fromList (0..100)) (0..100)


mapArray :: forall a. Array.Array a -> Array.Array a
mapArray array =
    Array.indexedMap (\i el -> 
        case (Array.get i array) of
            Just x -> x
            Nothing -> el
    ) array


tests :: forall e. TestUnit e
tests = test "Elm.Array\n" do
    test "Elm.Array.Creation" do
        assertEqual "empty" Array.empty (Array.fromList (Nil :: List Int))
        assertEqual "initialize" (Array.initialize 4 identity) (Array.fromList [0,1,2,3])
        assertEqual "initialize huge" 32768 (Array.length (Array.initialize 32768 identity))
        assertEqual "initialize 2" (Array.initialize 4 (\n -> n*n)) (Array.fromList [0,1,4,9])
        assertEqual "initialize 3" (Array.initialize 4 (always 0)) (Array.fromList [0,0,0,0])
        assertEqual "initialize Empty" (Array.initialize 0 identity) Array.empty
        assertEqual "initialize 4" (Array.initialize 2 (always 0)) (Array.fromList [0,0])
        assertEqual "initialize negative" (Array.initialize (-1) identity) Array.empty
        assertEqual "repeat" (Array.repeat 5 40) (Array.fromList [40,40,40,40,40])
        assertEqual "repeat 2" (Array.repeat 5 0) (Array.fromList [0,0,0,0,0])
        assertEqual "repeat 3" (Array.repeat 3 "cat") (Array.fromList ["cat","cat","cat"])
        assertEqual "fromList 1" (Array.fromList (Nil :: List Int)) Array.empty

    test "Elm.Array.Basics" do
        assertEqual "length" 3 (Array.length (Array.fromList [1,2,3]))
        assertEqual "length - Long" 10000 (Array.length (Array.repeat 10000 0))
        assertEqual "push" (Array.fromList [1,2,3]) (Array.push 3 (Array.fromList [1,2]))
        assertEqual "append" [42,42,81,81,81] (Array.toList (Array.append (Array.repeat 2 42) (Array.repeat 3 81)))
        assertEqual "appendEmpty 1" (1..33) (Array.toList (Array.append Array.empty (Array.fromList (1..33))))
        assertEqual "appendEmpty 2" (1..33) (Array.toList (Array.append (Array.fromList (1..33)) Array.empty))
        assertEqual "appendSmall 1" (1..33) (Array.toList (Array.append (Array.fromList (1..30)) (Array.fromList (31..33))))
        assertEqual "appendSmall 2" (1..33) (Array.toList (Array.append (Array.fromList (1..3)) (Array.fromList (4..33))))
        assertEqual "appendAndSlice" (0..100) (Array.toList holeArray)
    
    test "Elm.Array.Get and Set" do
        assertEqual "get" (Just 2) (Array.get 1 (Array.fromList [3,2,1]))
        assertEqual "get 2" (Nothing :: Maybe Int) (Array.get 5 (Array.fromList [3,2,1]))
        assertEqual "get 3" (Nothing :: Maybe Int) (Array.get (-1) (Array.fromList [3,2,1]))
        assertEqual "set 1" (Array.fromList [1,7,3]) (Array.set 1 7 (Array.fromList [1,2,3]))
        assertEqual "set 0" (Array.fromList [7,2,3]) (Array.set 0 7 (Array.fromList [1,2,3]))
        assertEqual "set 2" (Array.fromList [1,2,7]) (Array.set 2 7 (Array.fromList [1,2,3]))
        assertEqual "set 3" (Array.fromList [1,2,3]) (Array.set 3 7 (Array.fromList [1,2,3]))
        assertEqual "set -1" (Array.fromList [1,2,3]) (Array.set (-1) 7 (Array.fromList [1,2,3]))
    
    test "Elm.Array.Taking Arrays Apart" do
        assertEqual "toList" (toList [3,5,8]) (Array.toList (Array.fromList [3,5,8]))
        assertEqual "toList huge" 32768 (Data.List.length (Array.toList (Array.initialize 32768 identity)))
        
        assertEqual "toIndexedList" (toList [Tuple 0 "cat", Tuple 1 "dog"]) (Array.toIndexedList (Array.fromList ["cat","dog"]))
        assertEqual "bigIndexedList" 32768 (Data.List.length (Array.toIndexedList (Array.fromList (Data.Array.range 0 32767))))

        assertEqual "slice 1" (Array.fromList [0,1,2]) (Array.slice  0  3 (Array.fromList [0,1,2,3,4]))
        assertEqual "slice 2" (Array.fromList [1,2,3]) (Array.slice  1  4 (Array.fromList [0,1,2,3,4]))
        assertEqual "slice 3" (Array.fromList [1,2,3]) (Array.slice  1 (-1) (Array.fromList [0,1,2,3,4]))
        assertEqual "slice 4" (Array.fromList [2])     (Array.slice (-3) (-2) (Array.fromList [0,1,2,3,4]))
        assertEqual "slice 5" 63 (Array.length <| Array.slice 65 (65 + 63) <| Array.fromList (1..200))
   
    test "Elm.Array.Mapping and Folding" do
        assertEqual "map" (Array.fromList [1.0, 2.0, 3.0]) (Array.map sqrt (Array.fromList [1.0, 4.0, 9.0]))
        
        assertEqual "indexedMap 1" (Array.fromList [0,5,10]) (Array.indexedMap (*) (Array.fromList [5,5,5]))
        assertEqual "indexedMap 2" (0..99) (Array.toList (Array.indexedMap const (Array.repeat 100 0)))
        
        assertEqual "small indexed map" (Data.Array.range 0 (32 - 1)) (Array.toList <| mapArray <| Array.initialize 32 identity)
        assertEqual "large indexed map" (Data.Array.range 0 (32768 - 1)) (Array.toList <| mapArray <| Array.initialize 32768 identity)
        
        assertEqual "foldl 1" (toList [3,2,1]) (Array.foldl (:) Nil (Array.fromList [1,2,3]))
        assertEqual "foldl 2" 33 (Array.foldl (+) 0 (Array.repeat 33 1))
        
        assertEqual "foldr 1" 15 (Array.foldr (+) 0 (Array.repeat 3 5))
        assertEqual "foldr 2" (toList [1,2,3]) (Array.foldr (:) Nil (Array.fromList [1,2,3]))
        assertEqual "foldr 3" 53 (Array.foldr (-) 54 (Array.fromList [10,11]))
        
        assertEqual "filter" (Array.fromList [2,4,6]) (Array.filter (\x -> x % 2 == 0) (Array.fromList (1..6)))

