## Module DOM.Renderable

A module which ties together some general concepts for rendering things to the
DOM.

The general idea is that there are multiple approaches to taking some data
structure rendering something to the DOM as a result.  One common element is
the desire to be able to "update" the DOM based on changes (to avoid entirely
replacing the DOM every time).  This module provides a general interface for
that sort of thing.

The hope is that this enables a degree of mix-and-match between different
approaches. If each uses `DOM.Renderable`, and provides an instance for
`DOM.Renderable`, then you imagine using them together in a nice way, even if
they don't know about each other.  In any event, that's the hope.

So, this module doesn't provide very much functionality as such. It is mostly
meant as a possible rendezvous for other modules.

## Making something `Renderable`

The easy way to make something `Renderable` is to make it an instance
of the `Renderable` class. So, you just implement the `render` and `update`
methods ... see the discussion below for details.

If you can implement the `render` and `update` methods, but it's awkward to
create an instance, then you can also use the `makeDynamic` function to
make a `DynamicRenderable`.

Note that the best architecture is typically not to go straight from your
own specialized data types to `Renderable`, but instead to have a kind of
virtual-DOM layer in between -- it is the virtual DOM types that would be
`Renderable`, rather than your own more specialized types. So, you would
typically have a set of functions to convert from your specialized types
to a virtual-DOM layer, and it is those virtual DOM types which would have
a `Renderable` instance.

## Using a `Renderable`

To use a `Renderable`, you can, of course, just use the `render` or
`update` functions.

However, neither of those methods necessarily modify the DOM, so you'll
often want `renderIntoDOM` or `updateDOM` instead. (The `update` method
may simply modify the node you pass in, but it may instead return a new node ...
so you need to call `updateDOM` if you want to be sure to, well, update
the DOM).

## Making it more dynamic

Now, a `Renderable` instance is parameterized by the type of the original
data. This is a shame, in a way, since you might want to do things like keep
a list of `Renderable` things -- that is, a list of things that are all
`Renderable`, but of different types. Furthermore, you might, in traversing
a virtual DOM structure, want to use `update` in a situation where the previous
`Renderable` may or may not have had the same underlying type.

To facilitate this sort of thing, we also have a `DynamicRenderable` type which,
in effect, "erases" the type of the underlying data. This leaves you with a
"simple" type that you can use in lists etc. Even better, the `DynamicRenderable`
is itself an instance of `Renderable` -- isn't that nice? So you can use the usual
functions, even if the underlying data type doesn't match.

To get a `DynamicRenderable` from a `Renderable`, take a look at `toDynamic`.
Alternative, you can also use `makeDynamic`, if it's awkward to make an instance.

#### `Renderable`

``` purescript
class Renderable a where
  render :: forall e. a -> Eff (canvas :: Canvas, dom :: DOM | e) Node
  update :: forall e. Rendered a -> a -> Eff (canvas :: Canvas, dom :: DOM | e) Node
```

A `Renderable` is somethng that knows how to render some data type as a DOM
node. If given a previous version of the data, and the previously rendered
DOM node, it may also know how to efficiently `update` that node.

* The `render` method takes some data and produces a newly-created DOM node.

* The `update` method takes a previously `Rendered` structure, with the previous
  data and the node previously created from that data.

  You can either modify the provided node "in place" or create a new one. If you
  modify the node in place, then you should return it. If you create a new node,
  you should return that.  It is the caller's job to stitch whatever you return
  into whatever larger structure is involved -- you shouldn't do anything outside
  of the DOM you are given.

  You can use the `defaultUpdate` function to implement `update` if you're in a
  situation where no optimization is possible.  It simply calls `render`,
  ignoring the previous data and node.

* So, calling `render` and `update` should produce the same results,
  so long as the `Rendered` parameter provided to `update` follows the rules.

  Now, it is always possible that some extraneous code has modified the DOM in
  the meantime. However, ideally you won't do that except in ways that your
  `Renderable` explicitly permits (and is able to detect or compensate for in its
  `update` method).

##### Instances
``` purescript
Renderable DynamicRenderable
```

#### `defaultUpdate`

``` purescript
defaultUpdate :: forall a e. Renderable a => Rendered a -> a -> Eff (canvas :: Canvas, dom :: DOM | e) Node
```

A default implementation of `update`, which just calls `render` on the
new data, ignoring the previous state.

#### `Rendered`

``` purescript
type Rendered a = { value :: a, result :: Node }
```

A record that associates some data with the DOM rendered from that data.  Note
that the users of this type assume that the `result` really was generated by
rendering the `value`. If this is not true, behaviour is undefined.

#### `DynamicRenderable`

``` purescript
newtype DynamicRenderable
```

##### Instances
``` purescript
Renderable DynamicRenderable
```

#### `toDynamic`

``` purescript
toDynamic :: forall a. Renderable a => a -> DynamicRenderable
```

Given a `Renderable` value, return a dynamic value that can be used
without knowing the type of the original value.

#### `makeDynamic`

``` purescript
makeDynamic :: forall a. (forall e. a -> Eff (canvas :: Canvas, dom :: DOM | e) Node) -> (forall e. Rendered a -> a -> Eff (canvas :: Canvas, dom :: DOM | e) Node) -> a -> DynamicRenderable
```

An alternative to `toDynamic`, in cases where you don't have a
`Renderable` instance, but you do have the necessary functions.

#### `Position`

``` purescript
data Position
  = BeforeFirstChild
  | AfterLastChild
  | ReplacingChildren
  | ReplacingItself
```

Indicate a position for rendering via `renderIntoDOM`.

#### `renderIntoDOM`

``` purescript
renderIntoDOM :: forall e a. Renderable a => Position -> Node -> a -> Eff (canvas :: Canvas, dom :: DOM | e) (Rendered a)
```

#### `updateDOM`

``` purescript
updateDOM :: forall e a. Renderable a => Rendered a -> a -> Eff (canvas :: Canvas, dom :: DOM | e) (Rendered a)
```

Updates previously rendered data, replacing the originally rendered node in the DOM.


