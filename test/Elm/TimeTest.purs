module Test.Elm.TimeTest (tests) where

import Test.Unit
import Test.Unit.Console

import qualified Elm.Time as Time
import qualified Data.Time as DT
import Prelude (bind, Eq)
import Elm.Basics ((<|), (==), (*))


assertEqual :: forall a. (Eq a) => String -> a -> a -> Test ( testOutput :: TestOutput )
assertEqual name expected actual =
    assert name <| expected == actual 


tests :: Test ( testOutput :: TestOutput )
tests = do
    test "Time.Conversion Factors" do
        assert "Time.millisecond -> second" <| 500.0 * Time.millisecond == 0.5 * Time.second
        assert "Time.minute -> hour" <| 30.0 * Time.minute == 0.5 * Time.hour
        assert "Time.inMilliseconds" <| Time.inMilliseconds 500.0 == 500.0
        assert "Time.inSeconds" <| Time.inSeconds 5000.0 == 5.0
        assert "Time.inMinutes" <| Time.inMinutes 360000.0 == 6.0
        assert "Time.inHours" <| Time.inHours 3600000.0 == 1.0

    test "Time.Conversion with Data.Time" do
        assert "to milliseconds" <| Time.fromTime 500.0 == DT.Milliseconds 500.0
        assert "to seconds" <| Time.fromTime 500.0 == DT.Seconds 0.5
        assert "to minutes" <| Time.fromTime 60000.0 == DT.Minutes 1.0
        assert "to hours" <| Time.fromTime 3600000.0 == DT.Hours 1.0
        assert "from milliseconds" <| Time.toTime (DT.Milliseconds 500.0) == 500.0
        assert "from seconds" <| Time.toTime (DT.Seconds 0.5) == 500.0
        assert "from minutes" <| Time.toTime (DT.Minutes 1.0) == 60000.0
        assert "from hours" <| Time.toTime (DT.Hours 1.0) == 3600000.0

