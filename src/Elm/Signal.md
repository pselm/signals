# Notes on `Elm.Signal`, ports and `main`

In the case of other Elm modules, the Purescript implementation is a relatively
thin wrapper over existing Purescript modules. For instance, `Elm.Maybe` uses
the `Maybe` type from `Data.Maybe`, and just implements some extra Elm API.

`Signal`, on the other hand, is a little more complicated, for a variety of
reasons.

* There is nothing in Purescript that is quite like `Signal`, or at least not
  that lends itself to a simple wrapper.
  
  Now, there is the `purescript-signal` module. However, I wanted to match the
  Elm API even more closely than that does. Also, I wanted (ideally) to
  implement as much as possible in Purescript (rather than as foreign code).

* Signals in Elm get some assistance from the Elm runtime -- whereas Purescript
  does not have a runtime.

* The use of signals is essentially related to the way that `main` works,
  and things like ports, for which the Elm compiler provides a bit of "magic". 
  
  For instance, the execution of an Elm program has an implicit "setup the signal
  graph" phase, followed by a "run the signal graph" phase. This will need to
  be more explicit in Purescript.

Here are some thoughts on this cluster of issues.

## The `main` function

### The type of `main`

Elm accomodates a variety of types for the `main` function:

* `Graphics.Element`
* `Signal Graphics.Element`
* `Html`
* `Signal Html`

In Purescript, on the other hand, the idiomatic type of `main` is:

```purescript
main :: forall e. Eff e Unit
```

... though, you can specify particular types of effects if you like
(e.g. `main :: forall e. Eff (console :: CONSOLE | e) Unit`).

Now, in Elm terms, that (conceptually) translates into something like
this (since Elm doesn't subdivide its effects into types, but has
both an error and success type for the result):

```elm
main : Signal (Task () ())
```

So, conceptually, we've got two things to do here:

* Adapt between the idiomatic Elm `main` types and `Signal (Task () ())`
* Adapt between `Signal (Task () ())` and `forall e. Eff e Unit`

#### Elm's idiomatic `main` vs. `Signal (Task () ())`

This part is essentially just a matter of de-sugaring what Elm does for `main`.

Remember that the idiomatic Elm types for `main` are 

* `Graphics.Element`
* `Signal Graphics.Element`
* `Html`
* `Signal Html`

Now, `Graphics.Element` can be trivially made into `Signal Graphics.Element`
(and `Html` into `Signal Html`) via `Signal.constant`.

So, our task is to transform `Signal Html` (or `Signal Graphics.Element`) to
`Signal (Task () ())`.

This is, of course, just a `map`, where the type of the mapping function is
`Html -> Task () ()` or `Graphics.Element -> Task () ()`.

Now, the implementation of that function is easy to imagine -- it would create
a task that, given some HTML, updates the DOM in some way. Presumably, the root
function would need to specify *where* in the DOM the update should take place
-- this would probably be curried to result in the more specific function. And,
of course, this ought to use a virtual DOM for the sake of efficiency.

This "update the DOM" function essentially belongs in whatever module is
responsible for `Html`. If the appropriate function exists there, it won't be
hard to use it to construct a `Signal (Task () ())` for `main` to use. Essentially,
this will make visible some of the de-sugaring that Elm's main does, at the cost
of about 1 visible line of code in `main`.

#### `Signal (Task () ())` vs. `forall e. Eff e Unit`

Of course, having obtained an appropriate `Signal (Task () ())`, we still need
to be able to adapt from that to `forall e. Eff e Unit`.

Now, we can easily get from `Signal (Task () ())` to `Signal (Eff e Unit)` via
mapping functions (`Task.toAff` and `Aff.runAff` or `Aff.launchAff`, depending
on how we will handle exceptions -- which the `Task` type does not generate, so
we can probably just ignore them with `launchAff`).

So, we're ultimately going to need a function like this:

`runSignal :: Signal (Eff e Unit) -> Eff e Unit`

... where the returned `Eff` listens to the `Signal` and runs each effect as
it comes in. This is, in fact, exactly what the `purescript-signal` module
does.

So, in terms of what we have to program, we have two jobs so far (in addition
to implementing the `Signal` API on its own terms):

* In `Html`, implement a function something like this:

  `incrementalUpdate :: Node -> Html -> Task () ()` 

* Do something similar for `Graphics.Element`.

* In `Signal`, implement a function something like this:

  `runSignal :: forall e. Signal (Eff e Unit) -> Eff e Unit`

  ... and, I suppose, this trivial sugar:

  `runTasks :: Signal (Task () ()) -> Eff e Unit`

So, an Elm-in-Purescript `main` might look something like this, so far:

```purescript
main = do
    -- various things to set up the signal graph ... to be discussed
    tasks <- setupSignalOfTasks
    runTasks tasks
```


### Calling `main`

One complication I haven't considered yet is the way in which one calls `main`
from the outside world (i.e. from Javascript).

In Purescript, it works like this. Every `Eff` (which is what `main` is) is
an ordinary Javascript function. To perform the effect, you call the function.
So, to run the program, you just call the `main` function.

Now, Purescript compiles down to ordinary node modules, with exports etc. So,
assuming that you've exported `main`, you can find the function in the ordinary
way, and then call it to actually do whatever it does.

Elm handles this a bit differently. When an Elm program compiles, it generates
Javascript with a global `Elm` object, to which the various compiled modules
are attached.  To actually use your module, you either call
`Elm.fullscreen(Elm.YourModule)`, `Elm.embed(Elm.YourModule, domNode)`, or
`Elm.worker(Elm.YourModule)`

Let's consider each of these in turn.

#### fullscreen

In Purescript, the equivalent of `Elm.fullscreen(Elm.YourModule))` would be
something like this:

```javascript
var yourModule = require('yourModule');

yourModule.main();
```

Now, this would mean that your module would have to explicitly do some of
the magic that Elm's runtime does -- e.g.:

* Creating a DOM node
* Setting up the task that updates that DOM node with a `Signal Html`.

But that has already been discussed above, and we can add some nice sugar
for it.

#### embed

To implement something like `Elm.embed(Elm.YourModule, domNode)`, probably
the best approach is to provide an entry point on the Purescript side which
takes a parameter. There is, for instance, nothing to prevent you from
having multiple entry points, and the entry points can take parameters.

Consider, for instance, something like this:

```purescript
main :: forall e. String -> Eff e Unit
main domId = do
    tasks <- setupSignalOfTasks domId
    runTasks tasks
```

If you then call this from the Javascript side with the appropriate parameter,
it will work -- e.g.

```
var yourModule = require('yourModule');

yourModule.main("id")();
```

Note that the first function call returns the `Eff`, and the second function
call actually executes it.

Of course, you'll need to limit yourself to parameters which
match their Javascript counterparts (like string), or specify the parameter
as a `Json.Value` and decode it on the Purescript side (so that you can fail
fast if an inappropriate value is provided).

#### worker

What `worker` essentially does in Elm is set up and run the signal graph
without doing any of the "magic" for `Graphics.Element` or `Html`. So,
the Purescript equivalent would be to just call whatever function
(possibly `main`) does that for your module.

Now, the way one would actually use `worker` in Elm is via the `ports`
system, which requires its own discussion.


## The Ports System

Elm also has a "ports" system, which does a couple of things:

* Indicates that a `Task` or `Signal Task` should really be executed.
* Allows you to provide input from Javascript.
* Allows you to provide output to Javascript.

Let's consider each of these.

### Actually performing Tasks

Handled differently ... trivial ... channel to "main".

### Input from Javascript

port addUser : Signal (String, UserRecord)

So, we'd need to *expose* a function which Javascript can call
that talks to the input side of a Signal. This depends on what
the input side of a signal looks like ... also, where would
we hang it? Can we return things from what Javascript calls?
That is, would our "main" function return or not? It would be
easier if it did return ... then we could return something.

... automatically convert types.

Booleans and Strings – both exist in Elm and JS!
Numbers – Elm ints and floats correspond to JS numbers
Lists – correspond to JS arrays
Arrays – correspond to JS arrays
Tuples – correspond to fixed-length, mixed-type JS arrays
Records – correspond to JavaScript objects
Signals – correspond to event streams in JS
Maybes – Nothing and Just 42 correspond to null and 42 in JS
Json – Json.Encode.Value corresponds to arbitrary JSON

... we could have something that auto-converts between native JS and
foreign PS ... of course, not type-safe as such, but would at least
fail fast for *some* cases. Or, is there some type-classy thing
that can be done?

*or*

one-time port ... provide parameter to Elm.Fullscreen etc.

auto-implemented:

title sets the page title, ignoring empty strings
log logs messages to the developer console
redirect redirects to a different page, ignoring empty strings

### Output to Javascript

port requestUser : Signal String
port requestUser =
    signalOfUsersWeWantMoreInfoOn

Again, this should be conceptually easy *if* we can return something
from `main` -- then, it's just a question of constructing & returning.



