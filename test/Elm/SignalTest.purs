module Test.Elm.SignalTest (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (equal)

import Elm.Signal

import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))
import Data.Time.Duration (Milliseconds(..))

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)

import Prelude (Unit, pure, flip, bind, discard, class Eq, class Show, show, ($), (+), (-), (<), (>), (<>))


infixl 9 equals as ===

equals :: forall a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


later :: ∀ e. Aff e Unit -> Aff e Unit
later doThen = do
    delay (Milliseconds 1.0)
    doThen


tests :: ∀ e. TestSuite
    ( ref :: REF
    , delay :: DELAY
    , console :: CONSOLE
    , now :: NOW
    , avar :: AVAR
    , testOutput :: TESTOUTPUT
    | e)

tests = suite "Elm.Signal" do
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

        later do
            result <- liftEff $ current mbox.signal
            result === 24

    test "Signal.foldp" do
        sig <- setup do
            mbox <- mailbox 17
            folded <- foldp (+) 0 mbox.signal

            liftEff do
                send mbox.address 4
                send mbox.address 7
                send mbox.address 11

            pure folded

        later do
            result <- liftEff $ current sig
            result === 22

    test "Signal.filter" do
        sig <- setup do
            mbox <- mailbox 21
            sig1 <- filter (\x -> x < 10) 0 mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            pure sig2

        later do
            result <- liftEff $ current sig
            result === [7, 4]

    test "Signal.filterMap" do
        let
            toMaybe x =
                if x > 10
                    then Just x
                    else Nothing

        sig <- setup do
            mbox <- mailbox 21
            sig1 <- filterMap toMaybe 0 mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            pure sig2

        later do
            result <- liftEff $ current sig
            result === [11, 23]

    test "Signal.map" do
        sig <- setup do
            mbox <- mailbox 21
            sig1 <- map show mbox.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox.address 4
                send mbox.address 23
                send mbox.address 11
                send mbox.address 7

            pure sig2

        later do
            result <- liftEff $ current sig
            result === ["7", "11", "23", "4"]

    test "Signal.map2" do
        sig <- setup do
            mbox1 <- mailbox 21
            mbox2 <- mailbox 17
            sig1 <- map2 (-) mbox1.signal mbox2.signal
            sig2 <- foldp cons [] sig1

            liftEff do
                send mbox1.address 25
                send mbox2.address 23
                send mbox2.address 11
                send mbox1.address 12

            pure sig2

        later do
            result <- liftEff $ current sig
            result === [1, 14, 2, 8]

    test "Signal.map3" do
        let
            func3 a b c = a <> b <> c

        sig <- setup do
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

            pure sig2

        later do
            result <- liftEff $ current sig
            result === ["gfz", "gfc", "dfc", "dec", "dbc"]

    test "Signal.map4" do
        let
            func4 a b c d = a <> b <> c <> d

        sig <- setup do
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

            pure sig2

        later do
            result <- liftEff $ current sig
            result === ["gfzr", "gfzd", "gfcd", "dfcd", "decd", "dbcd"]

    test "Signal.map5" do
        let
            func4 a b c d e = a <> b <> c <> d <> e

        sig <- setup do
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

            pure sig2

        later do
            result <- liftEff $ current sig
            result === ["gfzra", "gfzre", "gfzde", "gfcde", "dfcde", "decde", "dbcde"]

    test "Signal.sampleOn" do
        sig <- setup do
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

            pure sig2

        later do
            result <- liftEff $ current sig
            result === [9, 17, 17]

    test "Signal.dropRepeats" do
        sig <- setup do
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

            pure sig2

        later do
            result <- liftEff $ current sig
            result === [4, 7, 11, 4]

    test "Signal.merge" do
        sigs <- setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"

            sig1 <- merge mbox1.signal mbox2.signal
            sig2 <- foldp (<>) "" sig1

            -- This one depends on sig1
            sig3 <- map (_ <> "1") sig1

            -- Now, let's merge sig3 with sig1. Because they both
            -- update on the same pulse, which update gets kept should
            -- depend on the order we merge them.
            sig4 <- merge sig1 sig3
            sig5 <- merge sig3 sig1

            sig6 <- foldp (<>) "" sig4
            sig7 <- foldp (<>) "" sig5

            liftEff do
                send mbox1.address "b"
                send mbox1.address "c"
                send mbox2.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"

            pure {sig2, sig3, sig6, sig7}

        later do
            result <- liftEff $ current sigs.sig2
            result === "gfedcb"

            result2 <- liftEff $ current sigs.sig3
            result2 === "g1"

            result3 <- liftEff $ current sigs.sig6
            result3 === "gfedcb"

            result4 <- liftEff $ current sigs.sig7
            result4 === "g1f1e1d1c1b1"

    test "Signal.mergeMany" do
        sig <- setup do
            mbox1 <- mailbox "a"
            mbox2 <- mailbox "b"
            mbox3 <- mailbox "c"

            sig1 <- mergeMany (mbox1.signal : mbox2.signal : mbox3.signal : Nil)
            sig2 <- foldp (<>) "" sig1

            liftEff do
                send mbox1.address "b"
                send mbox1.address "c"
                send mbox3.address "i"
                send mbox2.address "d"
                send mbox2.address "e"
                send mbox2.address "f"
                send mbox1.address "g"

            pure sig2

        later do
            result <- liftEff $ current sig
            result === "gfedicb"

    test "Signal.forwardTo" do
        sig <- setup do
            mbox <- mailbox "a"
            folded <- foldp (<>) "" mbox.signal

            let address = forwardTo mbox.address show

            liftEff do
                send address 6
                send address 7
                send address 8
                send address 9

            pure folded

        later do
            result <- liftEff $ current sig
            result === "9876"
