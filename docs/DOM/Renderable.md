## Module DOM.Renderable

This module is intened to co-ordinate multiple approaches to taking a data
structure which represents a virtual DOM, and rendering it to the real DOM.
One common element is the desire to be able to "update" the DOM based on
differences between the previous data and the new data (to avoid entirely
replacing the DOM every time). This module provides a high-level interface for
that sort of thing.

The hope is that this enables a degree of mix-and-match between different
approaches to a virtual DOM. If each uses `DOM.Renderable`, and provides an
instance for `DOM.Renderable`, then you imagine using them together in a nice
way, even if they don't know about each other.

## Making something `Renderable`

To make something `Renderable`, make it an instance of the `Renderable` class.
So, you just implement the `render` function (and possibly the `update`
function -- see below for details). Note that the instance is only responsible
for returning the new (or updated) DOM -- it is not responsible for stitching
it into the real DOM.

If you can implement the `render` and `update` methods, but it's awkward to
create an instance, then you can use the `makeAnyRenderable` function to create
an `AnyRenderable`.

Note that the best architecture is often not to go straight from your
own specialized data types to `Renderable`, but instead to have a kind of
virtual-DOM layer in between. It is the virtual DOM types that would be
`Renderable`, rather than your own more specialized types. So, you would
typically have a set of functions to convert from your specialized types
to a virtual-DOM layer, and it is those virtual DOM types which would have
a `Renderable` instance.

## Using a `Renderable`

To use a `Renderable`, you can either:

* use the instance functions (`render` or `update`) directly
* use `renderIntoDOM` or `updateDOM` to actually render or update the DOM
* use `renderOrUpdate` to automatically remember the previous data

### Using the instance directly.

To use a `Renderable`, you can, of course, just use the `render` or
`update` functions directly.

However, neither of those methods necessarily modify the DOM. The `render`
method only returns the new DOM, not yet inserted into the real DOM. The
`update` method may modify the node you pass in. but it may instead return a
new node.

So, you may prefer to use the helper methods instead.

### Using the helpers

You can use `renderIntoDOM` or `updateDOM` to actually modify the DOM.

The `renderIntoDOM` function is fairly straightforward -- it uses the
`render` function to produce the new DOM, and then inserts it into the
real DOM in the specified manner.

To use `updateDOM`, you'll need to remember the previous `Renderable` data somehow.
Now, a `Renderable` instance is parameterized by the type of the original
data. This is a shame, in a way, since you might want to do things like keep
a list of `Renderable` things -- that is, a list of things that are all
`Renderable`, but of different types. Furthermore, you might, in traversing
a virtual DOM structure, want to use `updateDOM` in a situation where the previous
`Renderable` may or may not have had the same underlying type.

To facilitate this sort of thing, we also have an `AnyRenderable` type
which, in effect, "erases" the type of the underlying `Renderable` data. This
leaves you with a "simple" `AnyRenderable` type that you can use in lists
etc. Even better, the `AnyRenderable` is itself an instance of `Renderable`.
So you can use the usual functions, even if the underlying data
type doesn't match.

To get an `AnyRenderable` from a `Renderable`, take a look at `toAnyRenderable`.
Alternatively, you can also use `makeAnyRenderable`, if it's awkward to use an instance.

### Automatically remembering the previous data

For the ultimate in convenience, there is the `renderOrUpdate` method. It stores
the provided data in the DOM itself. Thus, you don't need to do anything to
remember the previous data -- `renderOrUpdate` will use it if it finds it where
it put it.

#### `Renderable`

``` purescript
class Renderable a where
  render :: forall e. a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
  update :: forall e. Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
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

  Note that actually using `defaultUpdate` doesn't seem to be working, for
  some reason. However, you can manually write out a trivial update function
  in this way:

      update rendered = render

* So, calling `render` and `update` should produce the same results,
  so long as the `Rendered` parameter provided to `update` follows the rules.

  Now, it is always possible that some extraneous code has modified the DOM in
  the meantime. However, ideally you won't do that except in ways that your
  `Renderable` explicitly permits (and is able to detect or compensate for in its
  `update` method).

##### Instances
``` purescript
Renderable AnyRenderable
```

#### `defaultUpdate`

``` purescript
defaultUpdate :: forall a e. Renderable a => Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node
```

A default implementation of `update`, which just calls `render` on the
new data, ignoring the previous state.

Note that the compiler doesn't seem to be happy if you use this in implementing
an instance. However, you can manually write out a trivial `update` implementation
as follows:

      update rendered = render

#### `Rendered`

``` purescript
type Rendered a = { value :: a, result :: Node }
```

A record that associates some data with the DOM rendered from that data.  Note
that the users of this type assume that the `result` really was generated by
rendering the `value`. If this is not true, behaviour is undefined.

#### `AnyRenderable`

``` purescript
newtype AnyRenderable
```

Strips the underlying type from a `Renderable`.

You can create an `AnyRenderable` via `toAnyRenderable` or `makeAnyRenerable`.

It's useful if you want to keep values in a `List` (so they need to
to have the same type), or if you are remembering previous values
for use via `update`, and they may or may not have the same underlying
type.

There is a `Renderable` instance for `AnyRenderable`, so you can
use it with the regular functions.

##### Instances
``` purescript
Renderable AnyRenderable
```

#### `toAnyRenderable`

``` purescript
toAnyRenderable :: forall a. Renderable a => a -> AnyRenderable
```

Given a `Renderable` value, return a value that can be used
without knowing the type of the original data.

#### `makeAnyRenderable`

``` purescript
makeAnyRenderable :: forall a. (forall e. a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node) -> (forall e. Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Node) -> a -> AnyRenderable
```

An alternative to `toAnyRenderable`, in cases where you don't have a
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
renderIntoDOM :: forall e a. Renderable a => Position -> Node -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) (Rendered a)
```

Calls `render` on the provided value, and inserts the result into the DOM,
at the given position, relative to the provided node.

This is unlike `render`, in that `render` merely produces the new DOM,
without inserting it anywhere.

Once you've used `renderIntoDOM`, you could save the resulting `Rendered`
structure, and supply it to `updateDOM` when your data changes.

Or, for even more convenience, you could consider `renderOrUpdate`, which
keeps track of the previous data for you.

#### `updateDOM`

``` purescript
updateDOM :: forall e a. Renderable a => Rendered a -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) (Rendered a)
```

Updates previously data previously rendered via `renderToDOM`, replacing
the originally rendered node in the DOM. Returns a structure that can be
provided to `updateDOM` for future changes in the value.

#### `renderOrUpdate`

``` purescript
renderOrUpdate :: forall e a. Renderable a => Element -> a -> Eff (canvas :: CANVAS, dom :: DOM | e) Unit
```

Renders the provided `Renderable` and inserts the result as a child of the
provided `Element`, replacing all other chidlren.

This stores the provided `Renderable` on the `Element`. So, when you call this again
with the same `Element`, we may be able to use the renderable's `update`
method, to efficiently update the DOM.


