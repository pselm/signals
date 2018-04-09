module Test.Elm.TaskTest (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

import Elm.Task
import Prelude (flip, bind, discard, class Eq, class Show, ($), (+), (<$>), (<>), (>>>), show, pure, Unit)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Elm.Result (Result(..))
import Data.List (List(..), (:))
import Control.Monad.Aff (nonCanceler)
import Math (sqrt)


infixl 9 equals as ===

equals :: forall a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


a :: Task String String
a = succeed "a"

b :: Task String String
b = succeed "b"

c :: Task String String
c = succeed "c"

d :: Task String String
d = succeed "d"

e :: Task String String
e = succeed "e"

no :: Task String String
no = fail "No"


foreign import _evenAfter50 :: ∀ e. Int -> EffFnTask e String Int

evenAfter50 :: ∀ e. Int -> TaskE e String Int
evenAfter50 =
    _evenAfter50 >>> fromEffFnTask


tests :: forall e. TestSuite e
tests = suite "Elm.Task" do
    test "Task.succeed" do
        result <- toAff (succeed 42)
        result === Right 42 :: Either String Int

    test "Task.fail" do
        result <- toAff (fail "NOOO!")
        result === Left "NOOO!" :: Either String Int

    test "Task.map" do
        result <- toAff (sqrt <$> (succeed 9.0))
        result === Right 3.0 :: Either String Number

    test "Task.map2" do
        tt <- toAff (map2 (+) (succeed 9) (succeed 3))
        tf <- toAff (map2 (+) (succeed 9) (fail "No"))
        ft <- toAff (map2 (+) (fail "No") (succeed 9))
        ff <- toAff (map2 (+) (fail "No1") (fail "No2"))
        
        tt === Right 12 :: Either String Int
        tf === Left "No" :: Either String Int
        ft === Left "No" :: Either String Int
        ff === Left "No1" :: Either String Int

    test "Task.map3" do
        let func3 aa bb cc = aa <> bb <> cc

        ttt <- toAff (map3 func3 a b c)
        ttf <- toAff (map3 func3 a b no)
        tft <- toAff (map3 func3 a no c)
        ftt <- toAff (map3 func3 no b c)
        fff <- toAff (map3 func3 (fail "No1") (fail "No2") (fail "No3"))
        
        ttt === Right "abc"
        ttf === Left "No"
        tft === Left "No"
        ftt === Left "No"
        fff === Left "No1"

    test "Task.map4" do
        let func4 aa bb cc dd = aa <> bb <> cc <> dd

        tttt <- toAff (map4 func4 a b c d)
        tttf <- toAff (map4 func4 a b c no)
        ttft <- toAff (map4 func4 a b no c)
        tftt <- toAff (map4 func4 a no c d)
        fttt <- toAff (map4 func4 no b c d)
        ffff <- toAff (map4 func4 (fail "No1") (fail "No2") (fail "No3") (fail "No4"))
        
        tttt === Right "abcd"
        tttf === Left "No"
        ttft === Left "No"
        tftt === Left "No"
        fttt === Left "No"
        ffff === Left "No1"

    test "Task.map5" do
        let func5 aa bb cc dd ee = aa <> bb <> cc <> dd <> ee

        ttttt <- toAff (map5 func5 a b c d e)
        ttttf <- toAff (map5 func5 a b c d no)
        tttft <- toAff (map5 func5 a b c no e)
        ttftt <- toAff (map5 func5 a b no d e)
        tfttt <- toAff (map5 func5 a no c d e)
        ftttt <- toAff (map5 func5 no b c d e)
        fffff <- toAff (map5 func5 (fail "No1") (fail "No2") (fail "No3") (fail "No4") (fail "No5"))
        
        ttttt === Right "abcde"
        ttttf === Left "No"
        tttft === Left "No"
        ttftt === Left "No"
        tfttt === Left "No"
        ftttt === Left "No"
        fffff === Left "No1"

    test "Task.andMap" do
        let func3 aa bb cc = aa <> bb <> cc

        ttt <- toAff do
            func3 `map` a `andMap` b `andMap` c

        ttf <- toAff do
            func3 `map` a `andMap` b `andMap` no

        tft <- toAff do
            func3 `map` a `andMap` no `andMap` c

        ftt <- toAff do
            func3 `map` no `andMap` b `andMap` c

        fff <- toAff do
            func3 `map` (fail "No1") `andMap` (fail "No2") `andMap` (fail "No3")
        
        ttt === Right "abc"
        ttf === Left "No"
        tft === Left "No"
        ftt === Left "No"
        fff === Left "No1"

    test "Task.andThen" do
        tt <- toAff do
            a `andThen` (\n -> succeed $ n <> "b")

        tf <- toAff do
            a `andThen` (\n -> fail $ n <> "b")

        ft <- toAff do
            no `andThen` (\n -> succeed $ n <> "b")

        ff <- toAff do
            no `andThen` (\n -> fail $ n <> "b")
        
        tt === Right "ab" :: Either String String
        tf === Left "ab" :: Either String String
        ft === Left "No" :: Either String String
        ff === Left "No" :: Either String String

    test "Task.do notation" do
        tt <- toAff do
            n <- succeed "a"
            succeed $ n <> "b"

        tf <- toAff do
            n <- succeed "a"
            fail $ n <> "b"

        ft <- toAff do
            n <- fail "No"
            succeed $ n <> "b"

        ff <- toAff do
            n <- fail "No"
            fail $ n <> "b"

        tt === Right "ab" :: Either String String
        tf === Left "ab" :: Either String String
        ft === Left "No" :: Either String String
        ff === Left "No" :: Either String String

    test "Task.mapError" do
        let error = fail 42 :: Task Int Number
        let mapped = mapError show error :: Task String Number

        result <- toAff mapped
        result === Left "42"
        
        let error2 = succeed 42.0 :: Task Int Number
        let mapped2 = mapError show error2 :: Task String Number
        
        result2 <- toAff mapped2
        result2 === Right 42.0

    test "Task.onError" do
        let error = fail 42 :: Task Int String
        
        let recover = error `onError` (\int -> succeed (show int))
        let failDifferent = error `onError` (\int -> fail (show int))

        recoverResult <- toAff recover
        recoverResult === Right "42" :: Either Number String

        failedResult <- toAff failDifferent
        failedResult === Left "42" :: Either String String 

    test "Task.toMaybe" do
        error <- toAff do toMaybe (fail "file not found")
        error === Right Nothing :: Either Unit (Maybe Int)

        success <- toAff do toMaybe (succeed 42)
        success === Right (Just 42) :: Either Unit (Maybe Int)

    test "Task.fromMaybe" do
        error <- toAff do fromMaybe "file not found" Nothing
        error === Left "file not found" :: Either String Int

        success <- toAff do fromMaybe "file not found" (Just 42)
        success === Right 42 :: Either String Int

    test "Task.toResult" do
        error <- toAff do toResult (fail "file not found")
        error === Right (Err "file not found") :: Either Unit (Result String Int)

        success <- toAff do toResult (succeed 42)
        success === Right (Ok 42) :: Either Unit (Result String Int)

    test "Task.fromResult" do
        error <- toAff do toResult (fail "file not found")
        error === Right (Err "file not found") :: Either Unit (Result String Int)

        success <- toAff do toResult (succeed 42)
        success === Right (Ok 42) :: Either Unit (Result String Int)

    test "Task.sequence" do
        success <- toAff (sequence (succeed 1 : succeed 2 : succeed 3 : Nil))
        success === Right (1 : 2 : 3 : Nil) :: Either String (List Int)

        failure <- toAff (sequence (succeed 1 : fail "No" : succeed 3 : Nil))
        failure === Left "No"

    test "Task.spawn" do
        -- Not really testing the spawning as such ...
        task <- toAff do
            _ <- spawn (succeed 17)
            succeed 14

        task === Right 14 :: Either String Int

    test "Task.sleep" do
        -- Not really testing the sleeping as such ...
        task <- toAff do
            sleep 500.0
            succeed 42

        task === Right 42 :: Either String Int

    test "Task.makeTask" do
        errorTask <-
            toAff $
                makeTask \cb -> do
                    cb $ Right $ Left "No"
                    pure nonCanceler
        errorTask === Left "No" :: Either String Int

        successTask <-
            toAff $
                makeTask \cb -> do
                    cb $ Right $ Right $ 42
                    pure nonCanceler
        successTask === Right 42 :: Either String Int


    test "Task FFI" do
        success <- toAff do evenAfter50 12
        success === Right 12 :: Either String Int

        failure <- toAff do evenAfter50 13
        failure === Left "Not even or divisible by 3"


