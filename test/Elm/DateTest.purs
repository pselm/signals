module Test.Elm.DateTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Date 
import Prelude (bind, Eq, (<$>))
import Elm.Basics ((<|), (==))
import Data.Date (fromEpochMilliseconds, Date())
import Data.Time (Milliseconds(..))
import Elm.Result (toMaybe, Result(..))
import Data.Maybe


assertEqual :: forall a e. (Eq a) => String -> a -> a -> Assertion e 
assertEqual name expected actual =
    assert name <| expected == actual 


tests :: forall e. TestUnit e
tests = do
    test "Date.conversion" do
        assert "fromString good" <| toMaybe (fromString "May 27, 2014") == fromEpochMilliseconds (Milliseconds 1401166800000.0)
        assert "fromString bad"  <| toMaybe (fromString "bob") == (Nothing :: Maybe Date)

        assert "fromTime" <| Ok (fromTime 1318258080000.0) == fromString "2011-10-10T14:48:00Z"
        assert "toTime" <| (toTime <$> (fromString "2011-10-10T14:48:00Z")) == Ok 1318258080000.0

        let sample = fromString("Jun 23 1990 11:45:23")
        assert "year" <| (year <$> sample) == Ok 1990
        assert "month" <| (month <$> sample) == Ok June
        assert "day" <| (day <$> sample) == Ok 23
        assert "dayOfWeek" <| (dayOfWeek <$> sample) == Ok Saturday
        assert "hour" <| (hour <$> sample) == Ok 11
        assert "minute" <| (minute <$> sample) == Ok 45
        assert "second" <| (second <$> sample) == Ok 23
        assert "millisecond" <| (millisecond <$> sample) == Ok 0
