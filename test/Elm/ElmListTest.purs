module Test.Elm.ElmListTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.List

import Elm.Basics ((+), compare, (<|), (|>), (==), (%), (<), (>), (<=), (>=), toString, min, identity)
import Elm.Maybe (Maybe(..))
import Data.String ()
import Data.Tuple (Tuple(..))
import Prelude (bind, Eq, negate, (*), (/), (++), (-), flip, (&&))


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e
assertEqual name expected actual =
    assert name <| expected == actual


tests :: forall e. TestUnit e
tests = test "Elm.ElmList\n" do
    testListOfN 0
    testListOfN 1
    testListOfN 2
    testListOfN 1000


testListOfN :: forall e. Int -> TestUnit e 
testListOfN n =
    let
        xs =
            (1..n)

        xsOpp =
            ((-n)..(-1))

        xsNeg =
            foldl (:) Nil xsOpp -- assume foldl and (::) work

        zs =
            (0..n)

        sumSeq k =
            k * (k + 1) / 2

        xsSum =
            sumSeq n

        mid =
            n / 2

        halve :: Int -> Maybe Int
        halve x =
            if x % 2 == 0
                then Just (x / 2)
                else Nothing

     in
        test (toString n ++ " elements") do
            assertEqual "foldl order" (n) (foldl (\x acc -> x) 0 xs)
            assertEqual "foldl total" (xsSum) (foldl (+) 0 xs)

            assertEqual "foldlr order" (min 1 n) (foldr (\x acc -> x) 0 xs)
            assertEqual "foldlr total" (xsSum) (foldl (+) 0 xs)

            assertEqual "map identity" (xs) (map identity xs)
            assertEqual "map linear" (2..(n + 1)) (map ((+) 1) xs)

            assertEqual "isEmpty" (n == 0) (isEmpty xs)

            assertEqual "length" (n) (length xs)

            assertEqual "reverse" (xsOpp) (reverse xsNeg)

            assertEqual "member positive" (true) (member n zs)
            assertEqual "member negative" (false) (member (n + 1) xs)

            if n == 0
                then assertEqual "head Nothing" (Nothing :: Maybe Int) (head xs)
                else assertEqual "head Something" (Just 1) (head xs)

            assertEqual "filter none" (Nil :: List Int) (filter (\x -> x > n) xs)
            assertEqual "filter one" (n : Nil) (filter (\z -> z == n) zs)
            assertEqual "filter all" (xs) (filter (\x -> x <= n) xs)

            assertEqual "take none" (Nil :: List Int) (take 0 xs)
            assertEqual "take some" (0..(n - 1)) (take n zs)
            assertEqual "take all" (xs) (take n xs)
            assertEqual "take all+" (xs) (take (n + 1) xs)

            assertEqual "drop none" (xs) (drop 0 xs)
            assertEqual "drop some" (n : Nil) (drop n zs)
            assertEqual "drop all" (Nil :: List Int) (drop n xs)
            assertEqual "drop all+" (Nil :: List Int) (drop (n + 1) xs)

            assertEqual "repeat" (map (\x -> (-1)) xs) (repeat n (-1))

            assertEqual "append" (xsSum * 2) (append xs xs |> foldl (+) 0)

            assertEqual "(::)" (append ((-1) : Nil) xs) ((-1) : xs)

            assertEqual "concat" (append xs (append zs xs)) (concat (xs : zs : xs : Nil))

            assertEqual "intersperse"
                (Tuple (min (-(n - 1)) 0) xsSum)
                (intersperse (-1) xs |> foldl (\x (Tuple c1 c2) -> (Tuple c2 (c1 + x))) (Tuple 0 0))

            let left = partition (\x -> x > 0) xs
            assert "partition left" <| left.trues == xs && left.falses == (Nil :: List Int)
            
            let right = partition (\x -> x < 0) xs
            assert "patition right" <| right.trues == (Nil :: List Int) && right.falses == xs

            let split = partition (\x -> x > mid) xs
            assert "partition split" <| split.trues == ((mid + 1)..n) && split.falses == (1..mid) 

            assertEqual "map2 same length" (map ((*) 2) xs) (map2 (+) xs xs)
            assertEqual "map2 long first" (map (\x -> x * 2 - 1) xs) (map2 (+) zs xs)
            assertEqual "map2 short first" (map (\x -> x * 2 - 1) xs) (map2 (+) xs zs)

            assertEqual "unzip" (Tuple xsNeg xs) (map (\x -> Tuple (-x) x) xs |> unzip)

            assertEqual "filterMap none" (Nil :: List Int) (filterMap (\x -> Nothing :: Maybe Int) xs)
            assertEqual "filterMap all" (xsNeg) (filterMap (\x -> Just (-x)) xs)

            assertEqual "some" (1..mid) (filterMap halve xs)

            assertEqual "concatMap none" (Nil :: List Int) (concatMap (\x -> (Nil :: List Int)) xs)
            assertEqual "concatMap all" (xsNeg) (concatMap (\x -> ((-x) : Nil)) xs)

            assertEqual "indexedMap" (map2 Tuple zs xsNeg) (indexedMap (\i x -> (Tuple i (-x))) xs)

            assertEqual "sum" (xsSum) (sum xs)

            assertEqual "product" (0) (product zs)

            if n == 0
                then assertEqual "maximum nothing" (Nothing :: Maybe Int) (maximum xs)
                else assertEqual "maximum something" (Just n) (maximum xs)

            if n == 0
                then assertEqual "minimum something" (Nothing :: Maybe Int) (minimum xs)
                else assertEqual "minimum nothing" (Just 1) (minimum xs)

            assertEqual "all false" (false) (all (\z -> z < n) zs)
            assertEqual "all true" (true) (all (\x -> x <= n) xs)

            assertEqual "any false" (false) (any (\x -> x > n) xs)
            assertEqual "any true" (true) (any (\z -> z >= n) zs)

            assertEqual "sort sorted" (xs) (sort xs)
            assertEqual "sort unsorted" (xsOpp) (sort xsNeg)

            assertEqual "sortBy sorted" (xsNeg) (sortBy negate xsNeg)
            assertEqual "sortBy unsorted" (xsNeg) (sortBy negate xsOpp)

            assertEqual "sortWith sorted" (xsNeg) (sortWith (flip compare) xsNeg)
            assertEqual "sortWith unsorted" (xsNeg) (sortWith (flip compare) xsOpp)

            assertEqual "scanl" (0 : map sumSeq xs) (scanl (+) 0 xs)

