module Test.Elm.SignalTest (tests) where

import Test.Unit
import Test.Unit.Assert

import Elm.Signal
import Prelude (flip, bind, Eq, Show, show, ($), (+), (-), (<), (>), (++))
import Control.Monad.Eff.Class
import Control.Monad.Aff (later, later')
import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))


infixl 9 ===

(===) :: forall a e. (Eq a, Show a) => a -> a -> Assertion e
(===) = flip equal


-- For some reason, actually writing out the type here doesn't work,
-- even if I use what the compiler infers, which is:
--
-- Aff ( testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER | _18) Unit
--
-- Now, it (strangely) hasn't inferred a forall ...
tests :: _
tests = do
    graph <- liftEff $ makeGraph

    test "Signal.constant" do
        signal <- liftEff $ constant graph 17
        value <- liftEff $ current signal

        value === 17


    test "Signal.mailbox" do
        mbox <- liftEff $ mailbox graph 32
        
        value <- liftEff $ current mbox.signal
        value === 32

        liftEff (send mbox.address 24)

        later' 1 do
            result <- liftEff $ current mbox.signal
            result === 24
    

    test "Signal.foldp" do
        mbox <- liftEff $ mailbox graph 17    
        sig <- liftEff $ foldp (+) 0 mbox.signal

        liftEff do
            send mbox.address 4
            send mbox.address 7
            send mbox.address 11

        later' 1 do
            result <- liftEff $ current sig
            result === 22

    test "Signal.filter" do
        mbox <- liftEff $ mailbox graph 21
        sig1 <- liftEff $ filter (\x -> x < 10) 0 mbox.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox.address 4
            send mbox.address 23
            send mbox.address 11
            send mbox.address 7

        later' 1 do
            result <- liftEff $ current sig2
            result === [7, 4]

    test "Signal.filterMap" do
        let
            toMaybe x =
                if x > 10
                   then Just x
                   else Nothing

        mbox <- liftEff $ mailbox graph 21
        sig1 <- liftEff $ filterMap toMaybe 0 mbox.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox.address 4
            send mbox.address 23
            send mbox.address 11
            send mbox.address 7

        later' 1 do
            result <- liftEff $ current sig2
            result === [11, 23]
    
    test "Signal.map" do
        mbox <- liftEff $ mailbox graph 21
        sig1 <- liftEff $ map show mbox.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox.address 4
            send mbox.address 23
            send mbox.address 11
            send mbox.address 7

        later' 1 do
            result <- liftEff $ current sig2
            result === ["7", "11", "23", "4"]
    
    test "Signal.map2" do
        mbox1 <- liftEff $ mailbox graph 21
        mbox2 <- liftEff $ mailbox graph 17
        sig1 <- liftEff $ map2 (-) mbox1.signal mbox2.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox1.address 25
            send mbox2.address 23
            send mbox2.address 11
            send mbox1.address 12

        later' 1 do
            result <- liftEff $ current sig2
            result === [1, 14, 2, 8]

    test "Signal.map3" do
        let
            func3 a b c = a ++ b ++ c

        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph "b"
        mbox3 <- liftEff $ mailbox graph "c"

        sig1 <- liftEff $ map3 func3 mbox1.signal mbox2.signal mbox3.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox1.address "d"
            send mbox2.address "e"
            send mbox2.address "f"
            send mbox1.address "g"
            send mbox3.address "z"

        later' 1 do
            result <- liftEff $ current sig2
            result === ["gfz", "gfc", "dfc", "dec", "dbc"]

    test "Signal.map4" do
        let
            func4 a b c d = a ++ b ++ c ++ d

        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph "b"
        mbox3 <- liftEff $ mailbox graph "c"
        mbox4 <- liftEff $ mailbox graph "d"

        sig1 <- liftEff $ map4 func4 mbox1.signal mbox2.signal mbox3.signal mbox4.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox1.address "d"
            send mbox2.address "e"
            send mbox2.address "f"
            send mbox1.address "g"
            send mbox3.address "z"
            send mbox4.address "r"

        later' 1 do
            result <- liftEff $ current sig2
            result === ["gfzr", "gfzd", "gfcd", "dfcd", "decd", "dbcd"]

    test "Signal.map5" do
        let
            func4 a b c d e = a ++ b ++ c ++ d ++ e

        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph "b"
        mbox3 <- liftEff $ mailbox graph "c"
        mbox4 <- liftEff $ mailbox graph "d"
        mbox5 <- liftEff $ mailbox graph "e"

        sig1 <- liftEff $ map5 func4 mbox1.signal mbox2.signal mbox3.signal mbox4.signal mbox5.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox1.address "d"
            send mbox2.address "e"
            send mbox2.address "f"
            send mbox1.address "g"
            send mbox3.address "z"
            send mbox4.address "r"
            send mbox5.address "a"

        later' 1 do
            result <- liftEff $ current sig2
            result === ["gfzra", "gfzre", "gfzde", "gfcde", "dfcde", "decde", "dbcde"]

    test "Signal.sampleOn" do
        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph 17

        sig1 <- liftEff $ sampleOn mbox1.signal mbox2.signal
        sig2 <- liftEff $ foldp cons [] sig1
       
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

        later' 1 do
            result <- liftEff $ current sig2
            result === [9, 17, 17]
    
    test "Signal.dropRepeats" do
        mbox <- liftEff $ mailbox graph 21
        sig1 <- liftEff $ dropRepeats mbox.signal
        sig2 <- liftEff $ foldp cons [] sig1

        liftEff do
            send mbox.address 4
            send mbox.address 4
            send mbox.address 11
            send mbox.address 7
            send mbox.address 7
            send mbox.address 4

        later' 1 do
            result <- liftEff $ current sig2
            result === [4, 7, 11, 4]

    test "Signal.merge" do
        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph "b"

        sig1 <- liftEff $ merge mbox1.signal mbox2.signal
        sig2 <- liftEff $ foldp (++) "" sig1
       
        -- This one depends on sig1
        sig3 <- liftEff $ map (++ "1") sig1
       
        -- Now, let's merge sig3 with sig1. Because they both
        -- update on the same pulse, which update gets kept should
        -- depend on the order we merge them.
        sig4 <- liftEff $ merge sig1 sig3
        sig5 <- liftEff $ merge sig3 sig1

        sig6 <- liftEff $ foldp (++) "" sig4
        sig7 <- liftEff $ foldp (++) "" sig5

        liftEff do
            send mbox1.address "b"
            send mbox1.address "c"
            send mbox2.address "d"
            send mbox2.address "e"
            send mbox2.address "f"
            send mbox1.address "g"

        later' 1 do
            result <- liftEff $ current sig2
            result === "gfedcb"

            result2 <- liftEff $ current sig3
            result2 === "g1"

            result3 <- liftEff $ current sig6
            result3 === "gfedcb"

            result4 <- liftEff $ current sig7
            result4 === "g1f1e1d1c1b1"

    test "Signal.mergeMany" do
        mbox1 <- liftEff $ mailbox graph "a"
        mbox2 <- liftEff $ mailbox graph "b"
        mbox3 <- liftEff $ mailbox graph "c"

        sig1 <- liftEff $ mergeMany (mbox1.signal : mbox2.signal : mbox3.signal : Nil)
        sig2 <- liftEff $ foldp (++) "" sig1
       
        liftEff do
            send mbox1.address "b"
            send mbox1.address "c"
            send mbox3.address "i"
            send mbox2.address "d"
            send mbox2.address "e"
            send mbox2.address "f"
            send mbox1.address "g"

        later' 1 do
            result <- liftEff $ current sig2
            result === "gfedicb"

    test "Signal.forwardTo" do
        mbox <- liftEff $ mailbox graph "a"
        let address = forwardTo mbox.address show
        sig <- liftEff $ foldp (++) "" mbox.signal

        liftEff do
            send address 6
            send address 7 
            send address 8
            send address 9

        later' 1 do
            result <- liftEff $ current sig
            result === "9876"

