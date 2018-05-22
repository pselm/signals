## Module Elm.Signal

A *signal* is a value that changes over time.

TODO: Notes about usage.

#### `Signal`

``` purescript
newtype Signal a
```

A value that changes over time. So a `(Signal Int)` is an integer that is
varying as time passes, perhaps representing the current window width of the
browser. Every signal is updated at discrete moments in response to events in
the world.

#### `constant`

``` purescript
constant :: forall e m a. MonadEff (ref :: REF | e) m => a -> GraphState m (Signal a)
```

Create a signal that never changes. This can be useful if you need
to pass a combination of signals and normal values to a function:

    map3 view Window.dimensions Mouse.position (constant initialModel)

#### `foldp`

``` purescript
foldp :: forall e m a s. MonadEff (ref :: REF | e) m => (a -> s -> s) -> s -> Signal a -> GraphState m (Signal s)
```

Create a past-dependent signal. Each update from the incoming signal will
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

#### `map`

``` purescript
map :: forall e m a b. MonadEff (ref :: REF | e) m => (a -> b) -> Signal a -> GraphState m (Signal b)
```

Apply a function to a signal.

    mouseIsUp :: Signal Boolean
    mouseIsUp =
        map not Mouse.isDown

    main :: Signal Element
    main =
        map Graphics.Element.show Mouse.position

#### `map2`

``` purescript
map2 :: forall eff m a b r. MonadEff (ref :: REF | eff) m => (a -> b -> r) -> Signal a -> Signal b -> GraphState m (Signal r)
```

Apply a function to the current value of two signals. The function
is reevaluated whenever *either* signal changes. In the following example, we
figure out the `aspectRatio` of the window by combining the current width and
height.

    ratio : Int -> Int -> Float
    ratio width height =
        toFloat width / toFloat height

    aspectRatio : Signal Float
    aspectRatio =
        map2 ratio Window.width Window.height

#### `map3`

``` purescript
map3 :: forall eff m a b c r. MonadEff (ref :: REF | eff) m => (a -> b -> c -> r) -> Signal a -> Signal b -> Signal c -> GraphState m (Signal r)
```

#### `map4`

``` purescript
map4 :: forall eff m a b c d r. MonadEff (ref :: REF | eff) m => (a -> b -> c -> d -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> GraphState m (Signal r)
```

#### `map5`

``` purescript
map5 :: forall eff m a b c d e r. MonadEff (ref :: REF | eff) m => (a -> b -> c -> d -> e -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> GraphState m (Signal r)
```

#### `merge`

``` purescript
merge :: forall e m a. MonadEff (ref :: REF | e) m => Signal a -> Signal a -> GraphState m (Signal a)
```

Merge two signals into one. This function is extremely useful for bringing
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

#### `mergeMany`

``` purescript
mergeMany :: forall e m a. MonadEff (ref :: REF | e) m => List (Signal a) -> GraphState m (Signal a)
```

Merge many signals into one. This is useful when you are merging more than
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

#### `filter`

``` purescript
filter :: forall e m a. MonadEff (ref :: REF | e) m => (a -> Boolean) -> a -> Signal a -> GraphState m (Signal a)
```

Filter out some updates. The given function decides whether we should
*keep* an update. If no updates ever flow through, we use the default value
provided. The following example only keeps even numbers and has an initial
value of zero.

    numbers :: Signal Int

    isEven :: Int -> Bool

    evens :: Signal Int
    evens =
        filter isEven 0 numbers

#### `filterMap`

``` purescript
filterMap :: forall e m a b. MonadEff (ref :: REF | e) m => (a -> Maybe b) -> b -> Signal a -> GraphState m (Signal b)
```

Filter out some updates. When the filter function gives back `Just` a
value, we send that value along. When it returns `Nothing` we drop it.
If the initial value of the incoming signal turns into `Nothing`, we use the
default value provided. The following example keeps only strings that can be
read as integers.

    userInput : Signal String

    toInt : String -> Maybe Int

    numbers : Signal Int
    numbers =
        filterMap toInt 0 userInput

#### `dropRepeats`

``` purescript
dropRepeats :: forall e m a. MonadEff (ref :: REF | e) m => Eq a => Signal a -> GraphState m (Signal a)
```

Drop updates that repeat the current value of the signal.

    numbers : Signal Int

    noDups : Signal Int
    noDups =
        dropRepeats numbers

    --  numbers => 0 0 3 3 5 5 5 4 ...
    --  noDups  => 0   3   5     4 ...

#### `sampleOn`

``` purescript
sampleOn :: forall e m a b. MonadEff (ref :: REF | e) m => Signal a -> Signal b -> GraphState m (Signal b)
```

Sample from the second input every time an event occurs on the first input.
For example, `(sampleOn Mouse.clicks (Time.every Time.second))` will give the
approximate time of the latest click.

#### `Mailbox`

``` purescript
type Mailbox a = { address :: Address a, signal :: Signal a }
```

A `Mailbox` is a communication hub. It is made up of

  * an `Address` that you can send messages to
  * a `Signal` of messages sent to the mailbox

#### `mailbox`

``` purescript
mailbox :: forall e m a. MonadEff (ref :: REF, delay :: DELAY | e) m => a -> GraphState m (Mailbox a)
```

Create a mailbox you can send messages to. The primary use case is
receiving updates from tasks and UI elements. The argument is a default value
for the custom signal.

#### `Address`

``` purescript
newtype Address a
```

An `Address` points to a specific signal. It allows you to feed values into
signals, so you can provide your own signal updates.

The primary use case is when a `Task` or UI element needs to talk back to the
main part of your application.

#### `send`

``` purescript
send :: forall e a. Address a -> a -> Eff (ref :: REF, now :: NOW, delay :: DELAY, console :: CONSOLE | e) Unit
```

Send a message to an `Address`.

    type Action = Undo | Remove Int

    address : Address Action

    requestUndo : Task x ()
    requestUndo =
        send address Undo

The `Signal` associated with `address` will receive the `Undo` message
and push it through the Elm program.

#### `forwardTo`

``` purescript
forwardTo :: forall a b. Address b -> (a -> b) -> Address a
```

Create a new address. This address will tag each message it receives
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

#### `Message`

``` purescript
newtype Message e
```

A `Message` is like an envelope that you have not yet put in a mailbox.
The address is filled out and the envelope is filled, but it will be sent at
some point in the future.

#### `message`

``` purescript
message :: forall e a. Address a -> a -> Message e
```

Create a message that may be sent to a `Mailbox` at a later time.

Most importantly, this lets us create APIs that can send values to ports
*without* allowing people to run arbitrary tasks.

#### `timestamp`

``` purescript
timestamp :: forall e m a. MonadEff (ref :: REF | e) m => Signal a -> GraphState m (Signal (Tuple Time a))
```

Add a timestamp to any signal. Timestamps increase monotonically. When you
create `(timestamp Mouse.x)`, an initial timestamp is produced. The timestamp
updates whenever `Mouse.x` updates.

Timestamp updates are tied to individual events, so `(timestamp Mouse.x)` and
`(timestamp Mouse.y)` will always have the same timestamp because they rely on
the same underlying event (`Mouse.position`).

#### `output`

``` purescript
output :: forall e1 e2 m a. MonadEff (ref :: REF | e1) m => String -> (a -> Eff e2 Unit) -> Signal a -> GraphState m Unit
```

#### `setup`

``` purescript
setup :: forall e m a. MonadEff (ref :: REF, now :: NOW | e) m => GraphState m a -> m a
```

Setup the signal graph.

Basically, this provides a context in which you can do whatever you need to setup your
signal graph. So, you might have something like:

    main =
        setup do
            mbox <- mailbox "Initial value"
            map <- map ((<>) " concat") mbox
            ...

TODO: Write out a trivial working example once I have one!

Technically, this runs whatever commands you supply in the context of a newly initialized
signal graph, and returns whatever your commands return.

#### `current`

``` purescript
current :: forall e a. Signal a -> Eff (ref :: REF | e) a
```

Gets the current value of a signal.

Note that the normal Elm program architecture is to map a (combination of) signals
to a signal of tasks, and then have those tasks executed. So, normally you don't
need to "get" the current value ... the values simply flow to the tasks that need
to be executed.

However, there are cases in which it is useful to get a signal's value, so we provide
for it here. (For instance, it's handy when testing ...)

Note that some signal operations are async -- for instance, sending a value to a
`Mailbox` occurs async. However, `current` is *not* async. So, you'll need to
delay applying `current` in some cases to get the results you expect.

#### `DELAY`

``` purescript
data DELAY :: Effect
```

#### `delay`

``` purescript
delay :: forall eff a. Int -> Eff (delay :: DELAY | eff) a -> Eff (delay :: DELAY | eff) Unit
```

#### `Graph`

``` purescript
newtype Graph
```

A representation of the entirety of the signal graph. Among other things,

#### `GraphState`

``` purescript
type GraphState m a = StateT Graph m a
```

A computational context that tracks the state of the signal graph

#### `runSignal`

``` purescript
runSignal :: forall e1 e2 m. MonadEff (ref :: REF | e1) m => Signal (Eff e2 Unit) -> GraphState m Unit
```

Execute each effect as it arrives on a Signal.


