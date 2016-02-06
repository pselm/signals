module Elm.Signal
    ( Signal
    , constant
    , foldp
    , map, map2, map3, map4, map5
    , merge, mergeMany
    , filter, filterMap
    , dropRepeats
    , sampleOn
    , Mailbox, mailbox
    , Address, send, forwardTo
    , Message, message

    -- Everything below this line is not in the original Elm API.
    -- So, they are subject to experimentation, as I figure out
    -- how best to integrate this all.
    , setup
    , current
    , DELAY
    ) where

{-| An implmenetation of the Elm `Signal` API.

TODO: Write notes about usage.

-}

import Prelude
    ( class Eq, class Monad, Unit, unit, ($), (++), bind, (+)
    , pure, (==), (/=), (&&), (||), const, (<<<)
    )

import Control.Monad (when)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (error, CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, modifyRef, newRef)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.State.Class (gets, modify)
import Data.Array (cons)
import Elm.List (List(..), reverse)
import Data.List (foldM)
import Data.Exists (Exists, runExists, mkExists)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Date (Now, nowEpochMilliseconds)
import Data.Time (Milliseconds)
import Elm.Time (Time, toTime)
import Elm.Task (TaskE)
import Elm.Debug (crash)


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

As a first step, I've used `StateT` to make the `Graph` itself hidden state,
which essentially matches the Elm API.

-}


{- Time since epoch, used to give timestamps to events. -}
type Timestamp = Milliseconds


{- An ID for each signal ... made unique by the `Graph`.

Now, why do signals need an ID? It *seems* as though the only way we use the ID
is as a test for equality, when we're doing the `notify` pulses. This raises
a couple of questions:

* Could we handle signal equality in some other way? Note that we don't really
  need to treat two different signals as being possibly equal, at least for this
  purpose -- the question is really identity here, rather than equality. So,
  perhaps we could just use `refEq`?

* The pulse can only be transmitted by one of the parents -- so, really, we're
  just trying to distinguish between one parent and the other.
-}
type SignalID = Int


{-| A representation of the entirety of the signal graph. Among other things,
This is where we put things that are, in Elm, handled by the runtime (since we
have no runtime in Purescript.

* `guid` is a reference to a unique ID (so, I guess not a guid, since it is per-graph.

  Now, normally you'd have just one signal graph, but we don't prohibit multiple signal
  graphs ... indeed, the tests set up a signal graph for each test. So, it works.
  However, one problem is that you shouldn't connect two signals to each other if they
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

Note that we have mutable values here, even though we're using `StateT`, because I was
having trouble getting the `NotifyKid` callbacks to work if they were forced to run
inside the `StateT`. I'm not sure if there was a fundamental problem there, or just
something I couldn't figure out. In any event, this way we can provide each `Signal` with
the data it needs at runtime, without the `Signal` needing to consult the hidden graph
state again.
-}
type Graph =
    { guid :: SignalID
    , inputs :: Ref (Array UntypedSignal)
    , programStart :: Milliseconds
    , updateInProgress :: Ref Boolean
    }


{-| A computational context that tracks the state of the signal graph
as various signals are created and hooked up to each other.
-}
type GraphState m a = StateT Graph m a


{-| Setup the signal graph.

Basically, this provides a context in which you can do whatever you need to setup your
signal graph. So, you might have something like:

main =
    setup do
        mbox <- mailbox "Initial value"
        map <- map ((++) " concat") mbox
        ...

TODO: Write out a trivial working example once I have one!

Technically, this runs whatever commands you supply in the context of a newly initialized
signal graph, and returns whatever your commands return.
-}
setup :: forall e m a. (MonadEff (ref :: REF, now :: Now | e) m) => GraphState m a -> m a
setup cb = do
    programStart <- liftEff $ nowEpochMilliseconds
    inputs <- liftEff $ newRef []
    updateInProgress <- liftEff $ newRef false

    evalStateT cb
        { guid: 0
        , inputs
        , programStart
        , updateInProgress
        }


{-| Add the signal to the inputs. -}
registerInput :: forall e m a. (MonadEff (ref :: REF | e) m) => Signal a -> GraphState m Unit
registerInput node = do
    inputs <- getInputs
    modifyRefL inputs $ (cons (mkExists node))


{-| Get the inputs. -}
getInputs :: forall m. (Monad m) => GraphState m (Ref (Array UntypedSignal))
getInputs =
    gets _.inputs


{-| Get the inputs. -}
getProgramStart :: forall m. (Monad m) => GraphState m Milliseconds
getProgramStart =
    gets _.programStart


{-| Get a guid and increment it. -}
getGuid :: forall m. (Monad m) => GraphState m SignalID
getGuid = do
    modify \graph -> graph {guid = graph.guid + 1}
    gets _.guid


getUpdateInProgress :: forall m. (Monad m) => GraphState m (Ref Boolean)
getUpdateInProgress =
    gets _.updateInProgress


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
    -- which could possibly be handled in other ways.
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
    -- confine the mutability there. However, there may be some interesting
    -- type issues involved, since we'd want to use arrays of the polymorphic
    -- type, and yet I'm not sure `Exists` would help, because we really do
    -- want to match up the types in the end.
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
    -- its children. However, then the trick is making the tree from the graph
    -- object available as the 'pulse' is iterated.
    , kids :: Ref (Array UntypedSignal)

    -- It's actually only the inputs that need this ... should refactor. This is used
    -- by the `notify` method to start the pulse at all the inputs, whenever an
    -- input is updated.
    , inputs :: Ref (Array UntypedSignal)

    -- This also doesn't really belong here, but haven't figured out how to structure
    -- this best yet.
    , updateInProgress :: Ref Boolean

    -- I originally included this because I thought it might be needed for something.
    -- But it turns out that so far it isn't ... so perhaps could be removed.
    , role :: Role

    -- This represents what to do when we're notified by a parent that a change has
    -- occurred somewhere. Ideally, I'll simplify the method, and then actually
    -- walk the tree from the outside, rather than the inside.
    , notifyKids :: NotifyKid
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
addKidToParent ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    Signal a -> UntypedSignal -> GraphState m Unit

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
    modifyRefL parent.kids $ cons kid


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
broadcastToKids :: forall e a. Timestamp -> Boolean -> Signal a -> Eff (ref :: REF | e) Unit
broadcastToKids ts update (Signal node) = do
    kids <- readRef node.kids
    foreachE kids $
        runExists \(Signal kid') ->
            kid'.notifyKids ts update node.id $ mkExists (Signal kid')


{- Lift some functions, for the sake of convenience. -}

newRef' :: forall e m s. (MonadEff (ref :: REF | e) m) => s -> m (Ref s)
newRef' a = liftEff $ newRef a


readRef' :: forall e m s. (MonadEff (ref :: REF | e) m) => Ref s -> m s
readRef' a = liftEff $ readRef a


writeRef' :: forall e m s. (MonadEff (ref :: REF | e) m) => Ref s -> s -> m Unit
writeRef' a b = liftEff $ writeRef a b


modifyRefL :: forall e m s. (MonadEff (ref :: REF | e) m) => Ref s -> (s -> s) -> m Unit
modifyRefL a b = liftEff $ modifyRef a b


{- A function each signal defines as its callback. -}
type NotifyKid = forall e. Timestamp -> Boolean -> SignalID -> UntypedSignal -> Eff (ref :: REF | e) Unit


{- Makes a signal that receives values from outside. -}
input ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    String -> a -> GraphState m (Signal a)

input name base = do
    id <- getGuid
    value <- newRef' base
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    let
        -- Note that this actually does not do anything, and we don't expect it to be called.
        -- The values coming in from outside are actually handled in the top-level notify function.
        notifyKids :: NotifyKid
        notifyKids ts update parentID kid =
            pure unit

        node =
            Signal
                { id
                , name: "input-" ++ name
                , value
                -- Not that parents not really needed ... should redesign
                -- to reflect this.
                , parents: []
                , kids
                , inputs
                , updateInProgress
                , role: Input
                -- Note that this should never be called ... perhaps could
                -- redesign to factor it out
                , notifyKids
                }

    registerInput node
    pure node


{-| Create a signal that never changes. This can be useful if you need
to pass a combination of signals and normal values to a function:

    map3 view Window.dimensions Mouse.position (constant initialModel)
-}
constant :: forall e m a. (MonadEff (ref :: REF | e) m) => a -> GraphState m (Signal a)
constant = input "constant"


{- Note that this isn't actually used so far ... may end up handling it differently.

output :: forall a. String -> (a -> EffSignal Unit) -> Signal a -> EffSignal (Signal a)
output name handler (Signal parent) = do
    id <- getGuid parent.graph
    kids <- newRef []

    let
        notifyKids ts update parentID kid = do
            when update do
                value <- readRef parent.value
                handler value
            pure unit

        node =
            Signal
                { id
                , name: "output-" ++ name
                , parents: [mkExists (Signal parent)]
                -- Note that kids and value not really needed ... should
                -- redesign the type to reflect this.
                , kids
                , value: parent.value
                , notifyKids
                , role: Output
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
foldp ::
    forall e m a s. (MonadEff (ref :: REF | e) m) =>
    (a -> s -> s) -> s -> Signal a -> GraphState m (Signal s)

foldp func state (Signal parent) = do
    id <- getGuid
    value <- newRef' state
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    modifyRef value (func parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id
                , name: "foldp"
                , parents: [mkExists (Signal parent)]
                , kids
                , inputs
                , updateInProgress
                , value
                , role: Transform
                , notifyKids
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


timestamp ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    Signal a -> GraphState m (Signal (Tuple Time a))

timestamp (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    programStart <- getProgramStart
    value <- newRef' $ Tuple (toTime $ programStart) pv

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    writeRef value (Tuple (toTime ts) parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id
                , name: "timestamp"
                , parents: [mkExists (Signal parent)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
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
filter ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    (a -> Boolean) -> a -> Signal a -> GraphState m (Signal a)

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
filterMap ::
    forall e m a b. (MonadEff (ref :: REF | e) m) =>
    (a -> Maybe b) -> b -> Signal a -> GraphState m (Signal b)

filterMap pred base (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    value <- newRef' (fromMaybe base (pred pv))

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
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
                { id
                , name: "filterMap"
                , parents: [mkExists (Signal parent)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
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
map ::
    forall e m a b. (MonadEff (ref :: REF | e) m) =>
    (a -> b) -> Signal a -> GraphState m (Signal b)

map func (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    value <- newRef' $ func pv

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
            runExists \kid' -> do
                when update do
                    parentValue <- readRef parent.value
                    writeRef value (func parentValue)

                broadcastToKids ts update kid'

        node =
            Signal
                { id
                , name: "map"
                , parents: [mkExists (Signal parent)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
                }

    addKidToParent (Signal parent) (mkExists node)
    pure node


{- I'm using this as the basis for map2 through map5, rather than what the Elm code does.

Perhaps this should be exposed, as it could be useful?
-}
applySignal ::
    forall e m a b. (MonadEff (ref :: REF | e) m) =>
    Signal (a -> b) -> Signal a -> GraphState m (Signal b)

applySignal (Signal funcs) (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    fv <- readRef' funcs.value

    value <- newRef' $ fv pv

    -- We're keeping track of what we've been notified about each parent, as the pulses
    -- go through. On each pulse, we start at `Nothing` (not notified), and then switch
    -- to `Just true` or `Just false` to indicate that we've been notified, and what
    -- the value of 'update` was.
    --
    -- Once we've been updated by both parents, we do something, and reset the status.
    --
    -- Note that this assumes that every node only broadcasts to kids once per pulse.
    -- This is, in fact, true, but we'll have to keep it that way!
    parentUpdated <- newRef' (Nothing :: Maybe Boolean)
    funcsUpdated <- newRef' (Nothing :: Maybe Boolean)

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
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
                { id
                , name: "map"
                , parents: [mkExists (Signal parent), mkExists (Signal funcs)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
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
map2 ::
    forall eff m a b r. (MonadEff (ref :: REF | eff) m) =>
    (a -> b -> r) -> Signal a -> Signal b -> GraphState m (Signal r)

map2 func a b = do
    ab <- map func a
    applySignal ab b


{-|-}
map3 ::
    forall eff m a b c r. (MonadEff (ref :: REF | eff) m) =>
    (a -> b -> c -> r) -> Signal a -> Signal b -> Signal c -> GraphState m (Signal r)

map3 func a b c = do
    ab <- map func a
    bc <- applySignal ab b
    applySignal bc c


{-|-}
map4 ::
    forall eff m a b c d r. (MonadEff (ref :: REF | eff) m) =>
    (a -> b -> c -> d -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> GraphState m (Signal r)

map4 func a b c d = do
    ab <- map func a
    bc <- applySignal ab b
    cd <- applySignal bc c
    applySignal cd d


{-|-}
map5 ::
    forall eff m a b c d e r. (MonadEff (ref :: REF | eff) m) =>
    (a -> b -> c -> d -> e -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> GraphState m (Signal r)

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
sampleOn ::
    forall e m a b. (MonadEff (ref :: REF | e) m) =>
    Signal a -> Signal b -> GraphState m (Signal b)

sampleOn (Signal ticker) (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    value <- newRef' pv

    signalUpdated <- newRef' (Nothing :: Maybe Boolean)
    tickerUpdated <- newRef' (Nothing :: Maybe Boolean)

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
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
                { id
                , name: "sampleOn"
                , parents: [mkExists (Signal ticker), mkExists (Signal parent)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
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

-}
dropRepeats ::
    forall e m a. (MonadEff (ref :: REF | e) m, Eq a) =>
    Signal a -> GraphState m (Signal a)

dropRepeats (Signal parent) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    pv <- readRef' parent.value
    value <- newRef' pv

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
            runExists \kid' -> do
                myValue <- readRef value
                parentValue <- readRef parent.value

                let reallyUpdate = update && myValue /= parentValue

                when reallyUpdate $
                    writeRef value parentValue

                broadcastToKids ts reallyUpdate kid'

        node =
            Signal
                { id
                , name: "dropRepeats"
                , parents: [mkExists (Signal parent)]
                , role: Transform
                , notifyKids
                , value
                , inputs
                , kids
                , updateInProgress
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
merge ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    Signal a -> Signal a -> GraphState m (Signal a)

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
mergeMany ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    List (Signal a) -> GraphState m (Signal a)

mergeMany signalList =
    case reverse signalList of
        Nil ->
            crash "mergeMany was given an empty list!"

        Cons signal signals ->
            foldM merge signal signals


genericMerge ::
    forall e m a. (MonadEff (ref :: REF | e) m) =>
    (a -> a -> a) -> Signal a -> Signal a -> GraphState m (Signal a)

genericMerge tieBreaker (Signal left) (Signal right) = do
    id <- getGuid
    kids <- newRef' []
    inputs <- getInputs
    updateInProgress <- getUpdateInProgress

    lv <- readRef' left.value
    rv <- readRef' right.value

    value <- newRef' $ tieBreaker lv rv

    leftUpdated <- newRef' (Nothing :: Maybe Boolean)
    rightUpdated <- newRef' (Nothing :: Maybe Boolean)

    let
        notifyKids :: NotifyKid
        notifyKids ts update parentID =
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
                { id
                , name: "merge"
                , parents: [mkExists (Signal left), mkExists (Signal right)]
                , role: Transform
                , notifyKids
                , value
                , kids
                , inputs
                , updateInProgress
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
newtype Address a =
    Address (forall e. a -> Eff (ref :: REF, now :: Now, console :: CONSOLE, delay :: DELAY | e) Unit)


{- Updates the current value of the specified input `Signal`, and then broadcasts the
update throughout the signal graph.
-}
notify :: forall e a. Signal a -> a -> Eff (ref :: REF, now :: Now, console :: CONSOLE | e) Unit
notify (Signal signal) value = do
    -- Check for synchronous calls .... I suppose we could
    -- short-circuit and retry on the next tick?
    updateInProgress <- readRef signal.updateInProgress
    when updateInProgress $ error "The notify function has been called synchronously!"

    -- Indicate we're in progress
    writeRef signal.updateInProgress true

    -- Actually update the target signal's value
    writeRef signal.value value

    -- Notify all the inputs
    timestep <- nowEpochMilliseconds
    inputs <- readRef signal.inputs

    foreachE inputs (
        runExists \(Signal node') ->
            broadcastToKids timestep (signal.id == node'.id) (Signal node')
    )

    -- Stop checking for synchronous calls
    writeRef signal.updateInProgress false


{-| Create a mailbox you can send messages to. The primary use case is
receiving updates from tasks and UI elements. The argument is a default value
for the custom signal.
-}
mailbox ::
    forall e m a. (MonadEff (ref :: REF, delay :: DELAY | e) m) =>
    a -> GraphState m (Mailbox a)

mailbox base = do
    signal <- input "mailbox" base

    pure
        { signal: signal
        , address: Address (delay 0 <<< notify signal)
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
newtype Message e = Message (TaskE (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE | e) Unit Unit)


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
-- TODO: Need to make this a `Task`, actually.
send :: forall e a. Address a -> a -> Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE | e) Unit
send (Address actuallySend) value =
    actuallySend value


{-| Gets the current value of a signal.

Note that the normal Elm program architecture is to map a (combination of) signals
to a signal of tasks, and then have those tasks executed. So, normally you don't
need to "get" the current value ... the values simply flow to the tasks that need
to be executed.

However, there are cases in which it is useful to get a signal's value, so we provide
for it here. (For instance, it's handy when testing ...)

Note that some signal operations are async -- for instance, sending a value to a
`Mailbox` occurs async. However, `current` is *not* async. So, you'll need to
delay applying `current` in some cases to get the results you expect.
-}
current :: forall e a. Signal a -> Eff (ref :: REF | e) a
current (Signal signal) =
    readRef signal.value


{- I had used purescript-timers for this, but ran into problems unifying types
when running tests. It turned out that purescript-test-unit and
purescript-timers both use a `timer` effect, but specify a different type for
it. So, I think that was causing the problem.
-}
foreign import data DELAY :: !

foreign import delay :: forall eff a.
    Int ->
    Eff (delay :: DELAY | eff) a ->
    Eff (delay :: DELAY | eff) Unit
