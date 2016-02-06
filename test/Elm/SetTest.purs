module Test.Elm.SetTest (tests) where

import Test.Unit (TestUnit, Assertion, test)
import Test.Unit.Assert (assert)

import Elm.Set as Set
import Elm.Set (Set)
import Elm.Basics (Bool, (++), (+), (==), (<|), (<=))
import Prelude (bind, class Eq)
import Elm.List (List(..), (:), (..))


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual 


set :: Set Int
set = Set.fromList (1..100)


setPart1 :: Set Int
setPart1 = Set.fromList (1..50)


setPart2 :: Set Int
setPart2 = Set.fromList (51..100)


pred :: Int -> Bool
pred x = x <= 50


tests :: forall e. TestUnit e
tests = test "Elm.Set\n" do
    test "Set.build" do
        assertEqual "empty" (Set.fromList (Nil :: List Int)) (Set.empty)
        assertEqual "singleton" (Set.fromList (27 : Nil)) (Set.singleton 27)
        
        assertEqual "insert" (Set.fromList (27 : Nil)) (Set.insert 27 Set.empty)
        assertEqual "insert" (Set.fromList (27 : Nil)) (Set.insert 27 (Set.fromList (27 : Nil)))
        
        assertEqual "remove" Set.empty (Set.remove 27 (Set.singleton 27))
        assertEqual "remove not found" (Set.singleton 27) (Set.remove 28 (Set.singleton 27))

    test "Set.query" do
        assert "size of set of 100 elements" <| 100 == (Set.size set) 
        assert "size of empty Set" <| 0 == (Set.size Set.empty)
        
        assert "Simple filter" <| setPart1 == Set.filter pred set
        
        let partition = Set.partition pred set
        assert "Simple partition trues" <| partition.trues == setPart1
        assert "Simple partition falses" <| partition.falses == setPart2
        
        assertEqual "isEmpty true" true (Set.isEmpty Set.empty)
        assertEqual "isEmpty false" false (Set.isEmpty (Set.singleton 27))

        assertEqual "member 1" true (Set.member 90 set)
        assertEqual "member 2" false (Set.member 234 set)

    test "Set.transform" do
        assertEqual "map" (Set.singleton 3) (Set.map ((+) 1) (Set.singleton 2))
        
        assertEqual "foldl" "cba" (Set.foldl (++) "" (Set.fromList ("b" : "a" : "c" : Nil)))
        assertEqual "foldr" "abc" (Set.foldr (++) "" (Set.fromList ("b" : "a" : "c" : Nil)))
        
        assertEqual "toList" (3 : Nil) (Set.toList (Set.singleton 3))
        assertEqual "fromLlist" (Set.singleton 3) (Set.fromList (3 : Nil))

        assertEqual "union" (Set.fromList (1 : 2 : 3 : Nil)) (Set.union (Set.fromList (1 : 2 : Nil)) (Set.fromList (2 : 3 : Nil)))
        assertEqual "intersect" (Set.singleton 2) (Set.intersect (Set.fromList (1 : 2 : Nil)) (Set.fromList (2 : 3 : Nil)))
        assertEqual "diff" (Set.singleton 1) (Set.diff (Set.fromList (1 : 2 : Nil)) (Set.fromList (2 : 3 : Nil)))
