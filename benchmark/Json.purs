module Benchmark.Json where

import Prelude ((<$>), ($), (*), (<#>))
import Data.Array ((..))
import Data.List (List)
import Test.QuickCheck.Gen (vectorOf, chooseInt)
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Elm.Json.Decode as JD
import Elm.Json.Encode as JE
import Elm.Result (Result)


tryList :: Array Int -> Result String (List Int)
tryList input =
    JD.decodeValue (JD.list JD.int) (JE.array $ JE.int <$> input)


tryUnfoldable :: Array Int -> Result String (List Int)
tryUnfoldable input =
    JD.decodeValue (JD.unfoldable JD.int) (JE.array $ JE.int <$> input)


benchLists :: Benchmark
benchLists = mkBenchmark
    { slug: "JsonList"
    , title: "Decoding Lists"
    , sizes: (1..10) <#> (*100)
    , sizeInterpretation: "Number of elements"
    , inputsPerSize: 1
    , gen: \n -> vectorOf n (chooseInt 0 100) 
    , functions:
        [ benchFn "list" tryList
        , benchFn "unfoldable" tryUnfoldable
        ]
    }


main = runSuite [benchLists]

