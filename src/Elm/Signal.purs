module Elm.Signal
    ( Signal()
    , constant
    , foldp
    , map, map2, map3, map4, map5
    , merge, mergeMany
    , filter, filterMap
    , dropRepeats
    , sampleOn
    , Mailbox(), Address(), Message()
    , mailbox, send, message, forwardTo

    -- Everything below this line is not in the original Elm API.
    -- So, they are subject to experimentation, as I figure out
    -- how best to integrate this all.
    , current
    , Graph(), makeGraph
    ) where

{-| An implmenetation of the Elm `Signal` API.

TODO: Write notes about usage.

Note the presence of `Graph` in the API and its role.

-}

import Prelude
    ( Eq, Unit(), unit, ($), (++), bind, (+)
    , pure, (==), (&&), (||), const, void
    )

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Ref
import DOM.Timer (Timer(), delay)
import Data.Array (cons)
import Data.Exists
import Data.Tuple
import Data.List (List(..), reverse, foldM)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Date (nowEpochMilliseconds, Now())
import Data.Time (Milliseconds())
import Elm.Time (Time(), toTime)
import Elm.Debug (crash)
import Elm.Task (TaskE())


{- This is a relatively crude or naive translation of the Elm `Signal`
implementation from Javascript to Purescript. So, it's written in an imperative
style, originally more-or-less matching the original Javascript.

It's clear that there would be more functional ways of implementing this.  I
initially experimented with a few approaches, but it was difficult to know
exactly what would reproduce the (somewhat intricate) semantics of the Elm
code. And, it's important to do so, since Elm takes a particular approach to
FRP which we would want to reproduce here. So, I thought it might be useful to
stick close to the Elm code at first. One could then write tests and possibly
make it nicer.

However, I didn't want to do this via the FFI if I could avoid it, so I
"translated" the Elm Javascript implementation to Purescript. But, as I say,
somewhat crudely or naively.  I'm gradually working on making it more idiomatic
Purescript.
-}


{- Time since epoch, used to give timestamps to events. -}
type Timestamp = Milliseconds


{- An ID for each signal ... made unique by the `Graph`. 

Now, why do signals need an ID? It *seems* as though the only way we use the ID
is as a test for equality, when we're doing the `notify` pulses. This raises
a couple of questions:

* Could we handle signal equality in some other way? Note that we don't really
  need to treat two different signals as being possibly equal, at least for this
  purpose -- the question is really identity here, rather than equality.

* The pulse can only be transmitted by one of the parents -- so, really, we're
  just trying to distinguish between one parent and the other.
-}
type SignalID = Int


{-| A representation of the entirety of the signal graph. Among other things,
This is where we put things that are, in Elm, handled by the runtime (since we
have no runtime in Purescript.

* `guid` is a reference to a unique ID (so, I guess not a guid, since it is per-graph.

  Now, normally you'd have just one signal graph, but because we've made it explicit
  (in Elm it's essentially implicit), we don't prohibit it. And, it ought to work,
  though one problem is that you shouldn't connect two signals to each other if they
  belong to different signal graphs. I wonder whether we can prevent that at the type
  layer? Probably not -- though we could detect it at run-time and report it.

* `inputs` is a mutable array of all the input signals -- which are (essentially) those
  signals who are fed values from outside (either native event handlers or sending values
  to addresses), rather than transformations within the signal graph.

  I suppose that we could instead keep a list of *all* signals, and we could calculate which
  are the inputs easily enough. Or, we could note which signals are actually *used* -- that is,
  which individual signals are actually hooked up to something that will run the tasks that
  they generate -- and work backwards from that to figure out what the relevant inputs are.
  (Since we can walk the graph backwards from child to parent easily enough).

  Now, how are the inputs used? Essentially, when a change enters the signal graph, it changes
  the value of one input, but we "broadcast" the change to all inputs, who in turn broadcast
  to their kids. Why do we do that? If I understand it correctly, it is to "synchronize"
  the way that changes are propagated through the signal graph. For instance, a single change
  might actually update just one or both parents of a signal. We want to wait for each update
  to arrive before we handle the child signal. And there can be "diamonds" in the graph, as
  updates diverge and rejoin.

  There may be another way to handle all of that, though.

* `programStart` just records when the graph was created ... it's something that the Elm
  runtime otherwise does.

* `updateInProgress` is something which the Elm runtime tracks in order to show
  an error when dispatching updates while inside other updates.
-}
newtype Graph = Graph
    { guid :: Ref SignalID
    , inputs :: Ref (Array UntypedSignal)
    , programStart :: Milliseconds
    , updateInProgress :: Ref Boolean
    }


{-| Just a convenient unwrapper. -}
programStart :: Graph -> Milliseconds
programStart (Graph graph) =
    graph.programStart


{-| Another convenient wrapper. -}
readUpdateInProgress :: forall e. Graph -> Eff (ref :: REF | e) Boolean
readUpdateInProgress (Graph graph) =
    readRef graph.updateInProgress


{-| And another. -}
writeUpdateInProgress :: forall e. Graph -> Boolean -> Eff (ref :: REF | e) Unit
writeUpdateInProgress (Graph graph) value =
    writeRef graph.updateInProgress value


{-| And another. -}
readInputs :: forall e. Graph -> Eff (ref :: REF | e) (Array UntypedSignal)
readInputs (Graph graph) =
    readRef graph.inputs


{-| Add the signal to the inputs. -}
updateInputs :: forall e a. Graph -> Signal a -> Eff (ref :: REF | e) Unit
updateInputs (Graph graph) node = do
    modifyRef graph.inputs (cons (mkExists node))


{-| Get a guid and increment it. -}
getGuid :: forall e. Graph -> Eff (ref :: REF | e) SignalID
getGuid (Graph graph) = do
    modifyRef graph.guid (+ 1)
    readRef graph.guid


{-| Construct a representation of the whole signal graph.

This probably should be changed to be a kind of hidden state, using `StateT`
and modifying the API to run inside the `StateT`.
-}
makeGraph :: forall e. Eff (ref :: REF, now :: Now | e) Graph
makeGraph = do
    id <- newRef 0
    inputs <- newRef []
    now <- nowEpochMilliseconds  -- note Now effect
    updateInProgress <- newRef false

    pure $ Graph
        { guid: id
        , inputs: inputs
        , programStart: now
        , updateInProgress: updateInProgress
        }


{-| A value that changes over time. So a `(Signal Int)` is an integer that is
varying as time passes, perhaps representing the current window width of the
browser. Every signal is updated at discrete moments in response to events in
the world.
-}
newtype Signal a = Signal
    -- At the moment, producing a new Signal is inherently effectful, for
    -- a variety of reasons. One question is whether that could be changed.
    --
    -- One thing I considered was using a "free monad" approach ... that is,
    -- constructing a signal as pure data about what was wanted, and then
    -- turning that into something at execution time. However, the Elm API
    -- expects to get a "real" signal immediately ... it might be possible
    -- to work around that.

    -- Note comments about `id` above ... basically, just used for equality ...
    -- which could possibly be handled in other ways. This is one of the
    -- reasons why making a Signal is effectful, because the signal needs
    -- an effectful computation to get a guid. Perhaps that could be changed
    -- by only allowing signal creation in some monadic context that manages
    -- the guid as hidden state?
    { id :: SignalID

    -- I don't think the name is used at all, except perhaps for debugging
    , name :: String

    -- The current value of the signal. I believe it's true that we need to
    -- hang on to a value -- we can't treat them as ephemeral, because of
    -- the way that signals propagate. Or, the other way of looking at it
    -- is that this is a bit of memoization -- we can re-do the computation
    -- at specific moments, rather than whenever needed, because we've cached
    -- the last computation.
    --
    -- But, of course, creating the new reference is inherently effectful.
    --
    -- Perhaps this could be solved by storing all the memoized values in
    -- the Graph object? With keys based on the signal's id? That way, we'd
    -- confine the mutability there.
    , value :: Ref a

    -- Oddly enough, I'm pretty sure this is never actually used! Also, the
    -- way I've designed things, there can only ever be two parents -- I handle
    -- the `mapMany` case a little differently than the original Elm code, by
    -- creating signals of functions, rather than a list of multiple parents.
    --
    -- However, I won't remove this just yet, because the nice thing about it
    -- is that it is immutable. So, we 
    , parents :: Array UntypedSignal

    -- So, the difficulty here is that we need to keep track of kids to propagate
    -- changes, but of course the kids are created after the parent, so we're
    -- inherently looking at something mutable.
    -- 
    -- Perhaps one alternative would be to maintain a kind of mutable tree in the
    -- `Graph` object, so that the signal itself doesn't need to mutate to track
    -- its children.
    , kids :: Ref (Array UntypedSignal)

    -- I originally included this because I thought it might be needed for something.
    -- But it turns out that so far it isn't ... so perhaps could be removed.
    , role :: Role

    -- This represents what to do when we're notified by a parent that a change has
    -- occurred somewhere.
    , notify :: NotifyKid

    -- A reference back to the graph we're part of. Which makes moving some data to
    -- mutable stuff inside the graph feasible. Now, if we change things to that the
    -- graph is managed monadically as hidden state, then we might not need this
    -- reference any longer.
    , graph :: Graph
    }


{-| I thought we might need to keep track of the distinction for signals, but so
far it hasn't been used.
-}
data Role
    = Input
    | Output
    | Transform


{-| We somtimes want to put signals with different value types in an array etc.
So, we use existential types here to "hide" the value type.
-}
type UntypedSignal = Exists Signal


{-| Adds a child signal to a parent signal, so that we know how to broadcast
changes to the kids. The child is the `UntypedSignal`, since we want to put
them in an `Array` and they may have different value types.
-}
addKidToParent :: forall e a. Signal a -> UntypedSignal -> Eff (ref :: REF | e) Unit
addKidToParent (Signal parent) kid =
    -- We're using a `Ref` to update the list of children, which is sort of
    -- lame. Ideally, we'd register for callbacks in a way that didn't have
    -- to modify the parent. Note that the *only* place we use the `kids` is
    -- in `broadcastToKids`. So, if we can make that work without the `Ref`,
    -- that's all we would need.
    --
    -- One approach I considered was maintaining a *separate* mutable graph
    -- in the `Graph` object -- that way, at least each individual Signal
    -- itself wouldn't need to carry the mutable state.
    --
    -- Another approach would be to construct the graph of parents and children
    -- at some particular moment. Of course, the *parents* never change --
    -- that is immutable -- so, if we have all the children, we can compute
    -- the depdencies.
    modifyRef parent.kids $ cons kid


{-| Send a "pulse" to a Signal's children, indicating that something above it
in the Signal graph has changed.

* The `Timestamp` represents a timestamp for the change (i.e. the moment the
  change entered the signal graph).

* The `Boolean` represents whether this Signal has itself updated or not. Note
  that the value may not have actually *changed* -- that is, it could be the
  same value (see `dropRepeats` to avoid that). So, I suppose it's more whether
  the value has been *re-calculated* (even if it might be the same).

* The `Signal` is the signal whose children we're broadcasting to.
-}
broadcastToKids :: forall e a. Timestamp -> Boolean -> Signal a -> Eff (ref :: REF, now :: Now, timer :: Timer | e) Unit
broadcastToKids ts update (Signal node) = do
    kids <- readRef node.kids
    foreachE kids $
        runExists \(Signal kid') ->
            kid'.notify ts update node.id $ mkExists (Signal kid')


{- A convenience for the function each signal defines as its callback. -}
type NotifyKid = forall e. Timestamp -> Boolean -> SignalID -> UntypedSignal -> Eff (ref :: REF, timer :: Timer, now :: Now | e) Unit


{- Makes a signal that receives values from outside. -}
input :: forall e a. Graph -> String -> a -> Eff (ref :: REF | e) (Signal a)
input graph name base = do
    id <- getGuid graph
    kids <- newRef []
    value <- newRef base

    let
        -- Note that this actually does not do anything, and we don't expect it to be called.
        -- The values coming in from outside are actually handled in the top-level notify function.
        notify :: NotifyKid
        notify ts update parentID kid =
            pure unit

        node =
            Signal
                { id: id
                , name: "input-" ++ name
                , value: value
                -- Not that parents not really needed ... should redesign
                -- to reflect this.
                , parents: []
                , kids: kids
                , role: Input
                -- Note that this should never be called ... perhaps could
                -- redesign to factor it out
                , notify: notify
                , graph: graph
                }

    updateInputs graph node
    pure node


{-| Create a signal that never changes. This can be useful if you need
to pass a combination of signals and normal values to a function:

    map3 view Window.dimensions Mouse.position (constant initialModel)
-}
constant :: forall e a. Graph -> a -> Eff (ref :: REF | e) (Signal a)
constant graph = input graph "constant"


{- Note that this isn't actually used so far ... may end up handling it differently.

output :: forall a. String -> (a -> EffSignal Unit) -> Signal a -> EffSignal (Signal a)
output name handler (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []

    let
        notify :: NotifyKid
        notify ts update parentID kid = do
            when update do
                value <- readRef parent.value
                handler value
            pure unit

        node =
            Signal
                { id: id
                , name: "output-" ++ name
                , parents: [mkExists (Signal parent)]
                -- Note that kids and value not really needed ... should
                -- redesign the type to reflect this.
                , kids: kids
                , value: parent.value
                , notify: notify
                , role: Output
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node
-}


{-| Create a past-dependent signal. Each update from the incoming signal will
be used to step the state forward. The outgoing signal represents the current
state.

    clickCount :: Signal Int
    clickCount =
        foldp (\click total -> total + 1) 0 Mouse.clicks

    timeSoFar :: Signal Time
    timeSoFar =
        foldp (+) 0 (fps 40)

So `clickCount` updates on each mouse click, incrementing by one. `timeSoFar`
is the time the program has been running, updated 40 times a second.

Note: The behavior of the outgoing signal is not influenced at all by
the initial value of the incoming signal, only by updates occurring on
the latter. So the initial value of `sig` is completely ignored in
`foldp f s sig`.
-}
foldp :: forall e a s. (a -> s -> s) -> s -> Signal a -> Eff (ref :: REF | e) (Signal s)
foldp func state (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    value <- newRef state

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    modifyRef value (func parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id: id
                , name: "foldp"
                , parents: [mkExists (Signal parent)]
                , kids: kids
                , value: value
                , role: Transform
                , notify: notify
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


timestamp :: forall e a. Signal a -> Eff (ref :: REF | e) (Signal (Tuple Time a))
timestamp (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    value <- newRef $ Tuple (toTime $ programStart parent.graph) pv

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    writeRef value (Tuple (toTime ts) parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id: id
                , name: "timestamp"
                , parents: [mkExists (Signal parent)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


{-| Filter out some updates. The given function decides whether we should
*keep* an update. If no updates ever flow through, we use the default value
provided. The following example only keeps even numbers and has an initial
value of zero.

    numbers :: Signal Int

    isEven :: Int -> Bool

    evens :: Signal Int
    evens =
        filter isEven 0 numbers
-}
filter :: forall e a. (a -> Boolean) -> a -> Signal a -> Eff (ref :: REF | e) (Signal a)
filter isOk base signal =
    filterMap (\value ->
        if isOk value
           then Just value
           else Nothing
    ) base signal


{-| Filter out some updates. When the filter function gives back `Just` a
value, we send that value along. When it returns `Nothing` we drop it.
If the initial value of the incoming signal turns into `Nothing`, we use the
default value provided. The following example keeps only strings that can be
read as integers.

    userInput : Signal String

    toInt : String -> Maybe Int

    numbers : Signal Int
    numbers =
        filterMap toInt 0 userInput
-}
filterMap :: forall e a b. (a -> Maybe b) -> b -> Signal a -> Eff (ref :: REF | e) (Signal b)
filterMap pred base (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    value <- newRef (fromMaybe base (pred pv))

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' ->
                if update
                    then do
                        parentValue <- readRef parent.value
                        
                        case pred parentValue of
                            Just x -> do
                                writeRef value x
                                broadcastToKids ts true kid'

                            Nothing ->
                                broadcastToKids ts false kid'
                    
                    else
                        broadcastToKids ts false kid'

        node =
            Signal
                { id: id
                , name: "filterMap"
                , parents: [mkExists (Signal parent)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


{-| Apply a function to a signal.

    mouseIsUp :: Signal Boolean
    mouseIsUp =
        map not Mouse.isDown
    main :: Signal Element
    main =
        map Graphics.Element.show Mouse.position
-}
map :: forall e a b. (a -> b) -> Signal a -> Eff (ref :: REF | e) (Signal b)
map func (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    value <- newRef $ func pv

    let
        notify :: NotifyKid
        notify ts update parentID = 
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    writeRef value (func parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id: id
                , name: "map"
                , parents: [mkExists (Signal parent)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


applySignal :: forall e a b. Signal (a -> b) -> Signal a -> Eff (ref :: REF | e) (Signal b)
applySignal (Signal funcs) (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    fv <- readRef funcs.value
    value <- newRef $ fv pv

    -- We're keeping track of what we've been notified about each parent, as the pulses
    -- go through. On each pulse, we start at `Nothing` (not notified), and then switch
    -- to `Just true` or `Just false` to indicate that we've been notified, and what
    -- the value of 'update` was.
    --
    -- Once we've been updated by both parents, we do something, and reset the status.
    --
    -- Note that this assumes that every node only broadcasts to kids once per pulse.
    -- This is, in fact, true, but we'll have to keep it that way!
    parentUpdated <- newRef (Nothing :: Maybe Boolean)
    funcsUpdated <- newRef (Nothing :: Maybe Boolean)

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                when (parentID == funcs.id) $
                    writeRef funcsUpdated (Just update)

                when (parentID == parent.id) $
                    writeRef parentUpdated (Just update)

                pu <- readRef parentUpdated
                fu <- readRef funcsUpdated

                -- What we're doing is applying `(||)` to two `Maybe Boolean` values, and
                -- it does what you'd expect, if you think about it ... kind of neat!
                case pu || fu of
                    Just reallyUpdate -> do
                        -- both sides have been notified, so update if either side has updated
                        when reallyUpdate do
                            parentValue <- readRef parent.value
                            funcsValue <- readRef funcs.value

                            writeRef value $ funcsValue parentValue
                        
                        -- In either case, broadcast
                        broadcastToKids ts reallyUpdate kid'

                        -- And reset our state
                        writeRef parentUpdated Nothing
                        writeRef funcsUpdated Nothing

                    Nothing ->
                        -- Wait for the other notification to come in
                        pure unit

        node =
            Signal
                { id: id
                , name: "map"
                , parents: [mkExists (Signal parent), mkExists (Signal funcs)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    addKidToParent (Signal funcs) (mkExists node)
    
    pure node


{-| Apply a function to the current value of two signals. The function
is reevaluated whenever *either* signal changes. In the following example, we
figure out the `aspectRatio` of the window by combining the current width and
height.
    ratio : Int -> Int -> Float
    ratio width height =
        toFloat width / toFloat height
    aspectRatio : Signal Float
    aspectRatio =
        map2 ratio Window.width Window.height
-}
map2 :: forall eff a b r. (a -> b -> r) -> Signal a -> Signal b -> Eff (ref :: REF | eff) (Signal r)
map2 func a b = do
    ab <- map func a
    applySignal ab b


{-|-}
map3 :: forall eff a b c r. (a -> b -> c -> r) -> Signal a -> Signal b -> Signal c -> Eff (ref :: REF | eff) (Signal r)
map3 func a b c = do
    ab <- map func a
    bc <- applySignal ab b
    applySignal bc c


{-|-}
map4 :: forall eff a b c d r. (a -> b -> c -> d -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> Eff (ref :: REF | eff) (Signal r)
map4 func a b c d = do
    ab <- map func a
    bc <- applySignal ab b
    cd <- applySignal bc c
    applySignal cd d


{-|-}
map5 :: forall eff a b c d e r. (a -> b -> c -> d -> e -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Eff (ref :: REF | eff) (Signal r)
map5 func a b c d e = do
    ab <- map func a
    bc <- applySignal ab b
    cd <- applySignal bc c
    de <- applySignal cd d
    applySignal de e


{-| Sample from the second input every time an event occurs on the first input.
For example, `(sampleOn Mouse.clicks (Time.every Time.second))` will give the
approximate time of the latest click.
-}
sampleOn :: forall e a b. Signal a -> Signal b -> Eff (ref :: REF | e) (Signal b)
sampleOn (Signal ticker) (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    value <- newRef pv

    signalUpdated <- newRef (Nothing :: Maybe Boolean)
    tickerUpdated <- newRef (Nothing :: Maybe Boolean)

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                when (parentID == ticker.id) $
                    writeRef tickerUpdated (Just update)

                when (parentID == parent.id) $
                    -- This is the key logic of the sampling. We never trigger an
                    -- update from the parent ... we pretend that it didn't update.
                    -- So, we'll only actually update below when the ticker has
                    -- updated. Hence, sampling. But, we do record that the parent
                    -- was notified (via the `Just`).
                    writeRef signalUpdated (Just false)

                tu <- readRef tickerUpdated
                su <- readRef signalUpdated

                case tu || su of
                    Just reallyUpdate -> do
                        when reallyUpdate do
                            parentValue <- readRef parent.value
                            writeRef value parentValue

                        broadcastToKids ts reallyUpdate kid'

                        writeRef signalUpdated Nothing
                        writeRef tickerUpdated Nothing
                        
                    Nothing ->
                        pure unit

        node =
            Signal
                { id: id
                , name: "sampleOn"
                , parents: [mkExists (Signal ticker), mkExists (Signal parent)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal ticker) (mkExists node)
    addKidToParent (Signal parent) (mkExists node)

    pure node


{-| Drop updates that repeat the current value of the signal.

    numbers : Signal Int

    noDups : Signal Int
    noDups =
        dropRepeats numbers

    --  numbers => 0 0 3 3 5 5 5 4 ...
    --  noDups  => 0   3   5     4 ...

The signal should not be a signal of functions, or a record that contains a
function (you'll get a runtime error since functions cannot be equated).
-}
dropRepeats :: forall e a. (Eq a) => Signal a -> Eff (ref :: REF | e) (Signal a)
dropRepeats (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []
    pv <- readRef parent.value
    value <- newRef pv

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                myValue <- readRef value
                parentValue <- readRef parent.value

                let reallyUpdate = update && myValue == parentValue

                when reallyUpdate $
                    writeRef value parentValue

                broadcastToKids ts reallyUpdate kid'

        node =
            Signal
                { id: id
                , name: "dropRepeats"
                , parents: [mkExists (Signal parent)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: parent.graph
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


{-| Merge two signals into one. This function is extremely useful for bringing
together lots of different signals to feed into a `foldp`.

    type Update = MouseMove (Int,Int) | TimeDelta Float

    updates : Signal Update
    updates =
        merge
            (map MouseMove Mouse.position)
            (map TimeDelta (fps 40))

If an update comes from either of the incoming signals, it updates the outgoing
signal. If an update comes on both signals at the same time, the update provided
by the left input signal wins (i.e., the update from the second signal is discarded).
-}
merge :: forall e a. Signal a -> Signal a -> Eff (ref :: REF | e) (Signal a)
merge = genericMerge const


{-| Merge many signals into one. This is useful when you are merging more than
two signals. When multiple updates come in at the same time, the left-most
update wins, just like with `merge`.

    type Update = MouseMove (Int,Int) | TimeDelta Float | Click

    updates : Signal Update
    updates =
        mergeMany
            [ map MouseMove Mouse.position
            , map TimeDelta (fps 40)
            , map (always Click) Mouse.clicks
            ]
-}
mergeMany :: forall e a. List (Signal a) -> Eff (ref :: REF | e) (Signal a)
mergeMany signalList =
    case reverse signalList of
        Nil ->
            crash "mergeMany was given an empty list!"

        Cons signal signals ->
            foldM merge signal signals


genericMerge :: forall e a. (a -> a -> a) -> Signal a -> Signal a -> Eff (ref :: REF | e) (Signal a)
genericMerge tieBreaker (Signal left) (Signal right) = do
    id <- getGuid left.graph
    kids <- newRef []
    lv <- readRef left.value
    rv <- readRef right.value
    value <- newRef $ tieBreaker lv rv 

    leftUpdated <- newRef (Nothing :: Maybe Boolean)
    rightUpdated <- newRef (Nothing :: Maybe Boolean)

    let
        notify :: NotifyKid
        notify ts update parentID =
            runExists \kid' -> do
                when (parentID == left.id) $
                    writeRef leftUpdated (Just update)

                when (parentID == right.id) $
                    writeRef rightUpdated (Just update)

                lu <- readRef leftUpdated
                ru <- readRef rightUpdated

                case lu || ru of
                    Just reallyUpdate -> do
                        -- They have both been notified, so check which updated.
                        leftValue <- readRef left.value
                        rightValue <- readRef right.value

                        -- This is the Purescript equivalent of pattern-matching
                        -- on tuples, called "record punning" ... it's quite nice!
                        case {lu, ru} of
                            {lu: Just true, ru: Just true} ->
                                -- Both updated, so apply the tie-breaker
                                writeRef value $ tieBreaker leftValue rightValue

                            {lu: Just true, ru: _} ->
                                -- Just the left side updated
                                writeRef value leftValue

                            {lu: _, ru: Just true} ->
                                -- Just the right side updated
                                writeRef value rightValue

                            _ ->
                                -- Neither updated. So, we just broadcast.
                                pure unit

                        broadcastToKids ts reallyUpdate kid'

                        writeRef leftUpdated Nothing
                        writeRef rightUpdated Nothing

                    Nothing ->
                        pure unit

        node =
            Signal
                { id: id
                , name: "merge"
                , parents: [mkExists (Signal left), mkExists (Signal right)]
                , kids: kids
                , role: Transform
                , notify: notify
                , value: value
                , graph: left.graph
                }

    addKidToParent (Signal left) (mkExists node)
    addKidToParent (Signal right) (mkExists node)

    pure node


-- MAILBOXES

{-| A `Mailbox` is a communication hub. It is made up of

  * an `Address` that you can send messages to
  * a `Signal` of messages sent to the mailbox
-}
type Mailbox a =
    { address :: Address a
    , signal :: Signal a
    }


{-| An `Address` points to a specific signal. It allows you to feed values into
signals, so you can provide your own signal updates.

The primary use case is when a `Task` or UI element needs to talk back to the
main part of your application.
-}
newtype Address a = Address (forall e. a -> Eff (ref :: REF, timer :: Timer, now :: Now | e) Unit)


{- Updates the current value of the specified input `Signal`, and then broadcasts the
update throughout the signal graph.
-}
notify :: forall e a. Signal a -> a -> Eff (ref :: REF, now :: Now, timer :: Timer | e) Unit
notify (Signal signal) value = do
    -- Check for synchronous calls .... I suppose instead of crashing, we could
    -- short-circuit and retry on the next tick?
    updateInProgress <- readUpdateInProgress signal.graph
    when updateInProgress $ crash "The notify function has been called synchronously!"
    
    -- Indicate we're in progress
    writeUpdateInProgress signal.graph true
    
    -- Actually update the target signal's value
    writeRef signal.value value

    -- Notify all the inputs
    timestep <- nowEpochMilliseconds
    inputs <- readInputs signal.graph
    
    foreachE inputs $
        runExists \(Signal node') ->
            broadcastToKids timestep (signal.id == node'.id) (Signal node')

    -- Stop checking for synchronous calls
    writeUpdateInProgress signal.graph false


{-| Create a mailbox you can send messages to. The primary use case is
receiving updates from tasks and UI elements. The argument is a default value
for the custom signal.

Note: Creating new signals is inherently impure, so `(mailbox ())` and
`(mailbox ())` produce two different mailboxes.
-}
mailbox :: forall e a. Graph -> a -> Eff (ref :: REF, timer :: Timer, now :: Now | e) (Mailbox a)
mailbox graph base = do
    signal <- input graph "mailbox" base

    let
        send :: forall e1. a -> Eff (ref :: REF, now :: Now, timer :: Timer | e1) Unit 
        send value =
            void $
                delay 0
                    (notify signal)
                        value

    pure
        { signal: signal
        , address: Address send
        }


{-| Create a new address. This address will tag each message it receives
and then forward it along to some other address.

    type Action = Undo | Remove Int | NoOp

    actions : Mailbox Action
    actions = mailbox NoOp

    removeAddress : Address Int
    removeAddress =
        forwardTo actions.address Remove

In this case we have a general `address` that many people may send
messages to. The new `removeAddress` tags all messages with the `Remove` tag
before forwarding them along to the more general `address`. This means
some parts of our application can know *only* about `removeAddress` and not
care what other kinds of `Actions` are possible.
-}
forwardTo :: forall a b. Address b -> (a -> b) -> Address a
forwardTo (Address actuallySend) f =
    Address (\x -> actuallySend (f x))


{-| A `Message` is like an envelope that you have not yet put in a mailbox.
The address is filled out and the envelope is filled, but it will be sent at
some point in the future.
-}
newtype Message e = Message (TaskE (ref :: REF, now :: Now, timer :: Timer | e) Unit Unit)


{-| Create a message that may be sent to a `Mailbox` at a later time.

Most importantly, this lets us create APIs that can send values to ports
*without* allowing people to run arbitrary tasks.
-}
message :: forall e a. Address a -> a -> Message e
message (Address actuallySend) value =
    Message $ liftEff $ actuallySend value


{-| Send a message to an `Address`.

    type Action = Undo | Remove Int

    address : Address Action

    requestUndo : Task x ()
    requestUndo =
        send address Undo

The `Signal` associated with `address` will receive the `Undo` message
and push it through the Elm program.
-}
send :: forall e x a. Address a -> a -> TaskE (ref :: REF, now :: Now, timer :: Timer | e) x Unit
send (Address actuallySend) value =
    liftEff $ actuallySend value


{-| Gets the current value of a signal.

Note that the normal Elm program architecture is to map a (combination of) signals
to a signal of tasks, and then have those tasks executed. So, normally you don't
need to "get" the current value ... the values simply flow to the tasks that need
to be executed.

However, there are cases in which it is useful to get a signal's value, so we provide
for it here. (For instance, it's handy when testing ...)
-}
current :: forall e a. Signal a -> Eff (ref :: REF | e) a
current (Signal signal) =
    readRef signal.value
