module Test.Elm.TimeTest (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert)

import Elm.Time as Time
import Elm.Basics ((<|), (==), (*))
import Data.Time.Duration (Milliseconds(..), Hours(..), Seconds(..), Minutes(..))
import Prelude (discard, class Eq)


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Test e
assertEqual name expected actual =
    assert name <| expected == actual 


tests :: forall e. TestSuite e
tests = suite "Elm.Time" do
    test "Time.Conversion Factors" do
        assert "Time.millisecond -> second" <| 500.0 * Time.millisecond == 0.5 * Time.second
        assert "Time.minute -> hour" <| 30.0 * Time.minute == 0.5 * Time.hour
        assert "Time.inMilliseconds" <| Time.inMilliseconds 500.0 == 500.0
        assert "Time.inSeconds" <| Time.inSeconds 5000.0 == 5.0
        assert "Time.inMinutes" <| Time.inMinutes 360000.0 == 6.0
        assert "Time.inHours" <| Time.inHours 3600000.0 == 1.0

    test "Time.Conversion with Data.Time" do
        assert "to milliseconds" <| Time.fromTime 500.0 == Milliseconds 500.0
        assert "to seconds" <| Time.fromTime 500.0 == Seconds 0.5
        assert "to minutes" <| Time.fromTime 60000.0 == Minutes 1.0
        assert "to hours" <| Time.fromTime 3600000.0 == Hours 1.0
        assert "from milliseconds" <| Time.toTime (Milliseconds 500.0) == 500.0
        assert "from seconds" <| Time.toTime (Seconds 0.5) == 500.0
        assert "from minutes" <| Time.toTime (Minutes 1.0) == 60000.0
        assert "from hours" <| Time.toTime (Hours 1.0) == 3600000.0

