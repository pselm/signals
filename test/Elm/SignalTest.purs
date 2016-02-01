module Test.Elm.SignalTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Signal
import Prelude (flip, bind, Eq, Show, show, ($), (+), (-), (<), (>), (++))
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class
import Control.Monad.Aff (later')
import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))


infixl 9 ===

(===) :: forall a e. (Eq a, Show a) => a -> a -> Assertion e
(===) = flip equal


tests = test "Elm.Signal\n" do
    test "Signal.constant" do
        signal <- setup do
            constant 17

        value <- liftEff $ current signal
        value === 17

    test "Signal.mailbox" do
        mbox <- setup do
            mailbox 32

        value <- liftEff $ current mbox.signal
        value === 32

        liftEff (send mbox.address 24)

        later' 1 do
            result <- liftEff $ current mbox.signal
            result === 24

    test "Signal.foldp" do
        setup do
            mbox <- mailbox 17
            sig <- foldp (+) 0 mbox.signal

            liftEff do
                send mbox.address 4
                send mbox.address 7
                send mbox.address 11

            liftAff $ later' 1 do
                result <- liftEff $ current sig
                result === 22

    test "Signal.filter" do
        setup do
            mbox <- mailbox 21
            sig1 <- filter (\x -> x < 10) 0 mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === [7, 4]

    test "Signal.filterMap" do
        let
            toMaybe x =
                if x > 10
                then Just x
                else Nothing

        setup do
            mbox <- mailbox 21
            sig1 <- filterMap toMaybe 0 mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === [11, 23]

    test "Signal.map" do
        setup do
            mbox <- mailbox 21
            sig1 <- map show mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === ["7", "11", "23", "4"]

    test "Signal.map2" do
        setup do
            mbox1 <- mailbox 21
            mbox2 <- mailbox 17
            sig1 <- map2 (-) mbox1.signal mbox2.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox1.address 25
                send mbox2.address 23
                send mbox2.address 11
                send mbox1.address 12

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === [1, 14, 2, 8]

    test "Signal.map3" do
        let
            func3 a b c = a ++ b ++ c

        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"
            mbox3 <- mailbox "c"

            sig1 <- map3 func3 mbox1.signal mbox2.signal mbox3.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox1.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"
                send mbox3.address "z"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === ["gfz", "gfc", "dfc", "dec", "dbc"]

    test "Signal.map4" do
        let
            func4 a b c d = a ++ b ++ c ++ d

        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"
            mbox3 <- mailbox "c"
            mbox4 <- mailbox "d"

            sig1 <- map4 func4 mbox1.signal mbox2.signal mbox3.signal mbox4.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox1.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"
                send mbox3.address "z"
                send mbox4.address "r"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === ["gfzr", "gfzd", "gfcd", "dfcd", "decd", "dbcd"]

    test "Signal.map5" do
        let
            func4 a b c d e = a ++ b ++ c ++ d ++ e

        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"
            mbox3 <- mailbox "c"
            mbox4 <- mailbox "d"
            mbox5 <- mailbox "e"

            sig1 <- map5 func4 mbox1.signal mbox2.signal mbox3.signal mbox4.signal mbox5.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox1.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"
                send mbox3.address "z"
                send mbox4.address "r"
                send mbox5.address "a"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === ["gfzra", "gfzre", "gfzde", "gfcde", "dfcde", "decde", "dbcde"]

    test "Signal.sampleOn" do
        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox 17

            sig1 <- sampleOn mbox1.signal mbox2.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                -- Each of these should trigger a sample of 17
                send mbox1.address "b"
                send mbox1.address "c"

                -- These should essentially be lost
                send mbox2.address 27
                send mbox2.address 32

                -- But now we'll update and then sample
                send mbox2.address 9
                send mbox1.address "d"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === [9, 17, 17]

    test "Signal.dropRepeats" do
        setup do
            mbox <- mailbox 21
            sig1 <- dropRepeats mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 4
                send mbox.address 11
                send mbox.address 7
                send mbox.address 7
                send mbox.address 4

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === [4, 7, 11, 4]

    test "Signal.merge" do
        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"

            sig1 <- merge mbox1.signal mbox2.signal
            sig2 <- foldp (++) "" sig1

            -- This one depends on sig1
            sig3 <- map (++ "1") sig1

            -- Now, let's merge sig3 with sig1. Because they both
            -- update on the same pulse, which update gets kept should
            -- depend on the order we merge them.
            sig4 <- merge sig1 sig3
            sig5 <- merge sig3 sig1

            sig6 <- foldp (++) "" sig4
            sig7 <- foldp (++) "" sig5

            liftEff do
                send mbox1.address "b"
                send mbox1.address "c"
                send mbox2.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === "gfedcb"

                result2 <- liftEff $ current sig3
                result2 === "g1"

                result3 <- liftEff $ current sig6
                result3 === "gfedcb"

                result4 <- liftEff $ current sig7
                result4 === "g1f1e1d1c1b1"

    test "Signal.mergeMany" do
        setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"
            mbox3 <- mailbox "c"

            sig1 <- mergeMany (mbox1.signal : mbox2.signal : mbox3.signal : Nil)
            sig2 <- foldp (++) "" sig1

            liftEff do
                send mbox1.address "b"
                send mbox1.address "c"
                send mbox3.address "i"
                send mbox2.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"

            liftAff $ later' 1 do
                result <- liftEff $ current sig2
                result === "gfedicb"

    test "Signal.forwardTo" do
        setup do
            mbox <- mailbox "a"
            sig <- foldp (++) "" mbox.signal

            let address = forwardTo mbox.address show

            liftEff do
                send address 6
                send address 7
                send address 8
                send address 9

            liftAff $ later' 1 do
                result <- liftEff $ current sig
                result === "9876"
