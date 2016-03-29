# purescript-elm

Having done some [Elm](http://elm-lang.org) programming, I wanted to give
[Purescript](http://purescript.org) a try. I thought I would port one of my Elm
apps to Purescript, but quickly realized that there were a variety of little
differences between the Elm core libraries and their Purescript equivalents.
One possible approach would have been to modify my app.  However, it seemed to
me that it might be more interesting to port the Elm libraries to Purescript --
at least as a first step. I could then change the app to use more idiomatic
Purescript at my leisure.

Having started down that rabbit hole, I basically just became fascinated by how
Purescript does things -- and also fascinated by some of the inner workings of
Elm. So, I have continued to work away at it, with my original goals receding
far into the distance.

At this point, I would put this project in the "alpha" stage ... that is, it's
incomplete. However, some people could find parts of it useful, or at least
interesting.

The main things remaining to do are:

* The Graphics libraries (I've made a start on Elm.Graphics.Element)

* The virtual-dom and elm-html libraries.

* Making the initial setup of an Elm-like program as nice as possible.
  (That is, the construction of the signal graph, hooking up "ports", the
  graphics etc. -- the kind of thing which the Elm run-time facilitates).
  I have jotted down some [intial thoughts](src/elm/Signal.md) about this.

Note that I'm targeting Elm version 0.16, or version 3.0.0 of the core
Elm libraries. Once Elm version 0.17 comes out, I'll have to assess whether
it's easier to switch targets in-flight, or whether it would be easier to
complete work on Elm 0.16 and then work on the changes.


## Philosophy

In porting the Elm libraries to Purescript, I have followed the following
principles, roughly in order of priority.

1. Produce as much actual Purescript code as possible, with minimal use of
   custom Javascript. To this end, I've eliminated a fair bit of Elm's use of
   custom Javascript to implement core libraries.

2. Don't change the Elm function signatures, if it can be avoided. Porting Elm
   code should be as mechanical as possible.

3. Try to write idiomatic Purescript, re-using existing Purescript libraries
   where possible, and making use of Purescript idioms.


## Practical Matters

Each of the Elm libraries is prefixed by the `Elm.` namespace. So, to modify your
existing Elm code, you would simply add `Elm.` to the imports. And, of course,
you can always import `as` something to maintain any internal fully-qualified
references.

Eventually, I'll have some examples of porting existing Elm code. For the moment,
you can consult the test code, and the existing examples. They don't yet
support porting a full Elm program as well as I intend to in the future ...
it's just that you have to start somewhere. The idea is to make it as simple as
possible at the end of the day. This will require exposing some things which
the Elm run-time takes care of.


## Learning Purescript if you're familiar with Elm

For a general reference to the Purescript language, this page is very helpful:

https://github.com/purescript/purescript/wiki/Language-Guide

Here are a few notes on issues I ran into when performing the port, and how I
handled them. You may find some of this helpful if you are porting Elm code
to Purescript.


### The Type System

This is a big topic, of course -- I'll start with some of the mechanical
changes required.

Purescript uses `::` for type annotations (rather than Elm's `:`), and uses
`:` to concatenate lists (rather than Elm's `::`). So, you'll likely need to
do some changes there.

When using polymorphic types, Purescript requires you to use an explicit `forall`
to list the type variables before using them. So, an Elm signature like:

```elm
always : a -> b -> a
```

... would become this in Purescript:

```purescript
always :: forall a b. a -> b -> a
```

Or, you can actually use the unicode for-all character, which I've started doing
just because it looks nice:

```purescript
always :: ∀ a b. a -> b -> a
```


### Fancy Types

Purescript has what Elm folks sometimes refer to as "fancy types" -- that is,
things like type-classes, higher kinded types, rank-n polymorphism, and
probably others that I'm forgetting. I won't describe all of those things here,
but I do have a few opinions to offer.

1. Fancy types are great. Along with an elegant foreign function interface,
   they are among the best reasons to try Purescript.

2. However, they are genuinely tricky. If you remember how lost you initially
   felt when learning Elm (supposing it was your first intrduction to
   functional programming) -- well, that's how lost I felt again when starting
   with fancy types. The feeling goes away eventually, just like it did when
   you learned Elm, but it's a similar level of effort (at least, that's how it
   seemed to me).

3. You'll miss the Elm compiler's error messages. It's not that Purescript has
   bad error messages -- they are more-or-less typical in quality. But Elm's
   are so amazingly good -- you'll notice the difference. This is especially
   true when dealing with fancy types, as the fanciness does make constructing
   helpful error messages harder. Eventually, you do get the hang of what the
   messages probably signify.

4. The fancy types, once available, do tend to become a focus of the language.
   This is especially true when writing libraries for others to use. For
   instance, if you're writing an app for your own use, you can just pick (say)
   `List` for a data structure, and run with it. However, if you're writing a
   library, do you want to force your users to choose `List`? It's tempting to
   write your function signatures in terms of type-classes instead. And, of
   course, this is mostly a good thing -- after all, it's nice to be able to
   use a library without having to convert data. But, it does mean that once
   type-classes are in a language, they do tend to become a focus of library
   development. So, if you think you might have a better idea than
   type-classes, then it's easy to see why you might not want to start down
   that road.


### Tuples

Purescript does not have a literal syntax for Tuples. So, in places where
you used Tuples, there are two alternatives.

* There is a `Data.Tuple` type in
  [purescript-tuples](http://pursuit.purescript.org/packages/purescript-tuples).
  However, it is just an ordinary tagged union type. So you construct and pattern
  match it in the usual way -- there is no `,` operator to construct tuples.

  ```purescript
  tuple = Tuple 1 2

  case tuple of
      Tuple a b -> a
  ```

* Usually, it's better to use Purescript's record type. Essentially, if you
  have a Tuple2, just use a record where you name the first and second
  elements.  (You could even name them `fst` and `snd` if you don't have
  anything better at hand).

  ```purescript
  tuple = {
    fst: 1
    snd: 2
  }
  ```

  Note that Purescript records have something called "punning" (see below),
  which makes using them in this way pretty nice, actually.
  
  The only disadvantage is that Purescript doesn't auto-derive an `Eq` instance
  for records, so comparing them isn't as easy as it might be. In fact, you
  can't make an `Eq` instance for records at all ... you would have to construct
  a `newtype` to do that, in which case accessing the fields becomes somewhat
  more awkward (as you have to wrap and unwrap the newtype) ... anyway, it's
  a bit of a disadvantage.

In doing the conversion, I've sometimes used the `Tuple` type, and sometimes
converted to using records instead. In either case, you'll need to make some
modifications to your own code that uses Tuples.


### Booleans

The Purescript type is `Boolean`, rather than the Elm `Bool`. I've put a type
alias in `Elm.Basics` to cover that.

The Boolean literals are `true` and `false`, rather than Elm's `True` and `False`.


### Records

Purescript records are broadly similar to Elm records, with some differences
in syntax. 


#### Initialization

Initialization is via a `:` rather than `=`, e.g.

```purescript
let record =
    { x: 7
    , y: 32
    }
```

You don't get a constructor function automatically, but you can create one
with wildcards, e.g.:

```purescript
{ x: _, y: _} == \x y -> {x: x, y: y}
```

Generally speaking, Purescript gets a vast amount of use out of the lowly
underscore.


#### Access and accesor function

Acces is via the expected `record.x`.

There are also polymorphic accessor functions like Elm's, but instead of `.x`
you use `_.x` (which is kind of logical, if you think about it). If you were to
write out the type of `_.x`, it would be something like this:

```purescript
forall a b. { x :: a | b } -> a
```
... which you can read as: a function which, given a record with a field named
`x` of type `a`, and possibly other fields, returns something of type `a`
(which is, of course, the value of whatever was in field `x`).


#### Update and updater functions

Update looks like this, which is a little different than Elm, but basically
similar:

```purescript
record {
    x = 17
}
```

And, Purescript has a clever way of producing updater functions. To make a
function that updates the `x` field, you can do this:

```purescript
_ { x = _ } == \a b -> a { x = b } 
```

So, this is just like the literal syntax for record updates, but with wildcards
that become the parameters to the function. Isn't that nice? And, you can
fill in one of the wildcards if you like, to get a partially applied function.


#### Punning

Another thing I particularly like is record "punning". You can destructure a record
by mentioning the keys, and the keys get used as names. So, you can do
something like this:

```purescript
fromPolar :: {r :: Float, theta :: Float} -> {x :: Float, y :: Float}
fromPolar {r, theta} =
    { x: r * cos(theta)
    , y: r * sin(theta)
    }
```

And `r` and `theta` are used both as the field names of the record and as names
you can refer to. This makes substituting a record for a tuple relatively
painless.


### Unit

Elm uses a Tuple0 -- that is, `()` -- as a type (and value) when a value is
required but there is no natural value to supply.

Purescript has a `Unit` type for this purpose, which is inhabited by a `unit`
value.


### Numbers

Purescript uses `Number` for what Elm calls a `Float`. I've put a type alias
in `Elm.Basics` to cover that.

I believe that Elm distinguishes between `Int` and `Float` literals via type
inference.  Purescript instead looks at the form of the literal -- for
instance, `1` is an `Int` and `1.0` is a `Number`. So, you'll sometimes need to
add ".0" to numeric literals to signify that they are floats.

Purescript doesn't seem to have an exponention function which will accept an
`Int` or a `Number` -- the one in purescript-math only takes a `Number`.
However, Elm's `(^)` operator wants to take either. So, I've created a `Pow`
typeclass to accommodate this -- there may be a better way that I'm not
familiar with.

I think the Purescript unary `(-)` operator behaves a little differently from
Elm's -- at least, I find that I sometimes need to put something like
`(-7)` in parentheses to get the results I want.

Purescript actually enforces at run-time that an `Int` will have no fractional
part. I believe this is unlike Elm, where the distinction between `Float` and
`Int` is purely a question of types, and disappears at run-time. One
consequence is that Purescript essentially enforces that `Int` cannot be bigger
than 2147483647 nor smaller than -2147483648 -- the implementation of the
"fraction truncating" via `| 0` has the side-effect of enforcing those
boundaries. So, this can cause some subtle problems when converting Elm code
that uses integers outside that range.

One way of summarizing this is that the Purescript `Int` type is basically
an `Int32`. However, the Elm `Integer` type is essentially an `Int53`, in the
sense that you can safely use 53 bits before things like adding 1 don't work
quite right.

To deal with this, I've implemented an `Int53` type in `Elm.Int53`. It was
an interesting exercise ... by implementing the various numeric typeclasses,
one can re-use the familiar operators. And, one can then define functions
that work on multiple types -- essentially, you can treat `Int53` as a
reasonably first-class alternative to `Int`, when you need the extra bits.
This is, for instance, the case in the `Elm.Random` module.

### Imports

Purescript doesn't import anything by default. So, if you want something, you
have to import it manually. For instance, if you want `Elm.Basics`, you need
to `import Elm.Basics`.

A "plain" import statement imports everything in a module. So, in Purescript,
`import Data.List` is equivalent to the Elm `import List exposing (..)`.

If you're going to refer to something in fully-qualified way, it appears that
you don't actually have to import it at all, which is unlike Elm. However,
I've seen some discussion on the mailing list that this might be a warning
or an error in the future.

If you want to re-export something, you need to re-export a whole module. However,
it can be an aliased module name, and you can import specific symbols from other
modules into the the aliased module. So, you can do something like this, which
is kind of neat:

```purescript
module Elm.Basics 
    ( module Virtual
    )

import Data.Ord (max, min, clamp) as Virtual
import Global (isNaN) as Virtual
import Data.Tuple (Tuple(..), fst, snd, curry, uncurry) as Virtual
import Math (sqrt, e, pi, sin, cos, tan, acos, asin, atan, atan2) as Virtual
import Data.Int (round, floor) as Virtual
```

... and all of the individually imported names will be re-exported.


### List

I've used Purescript's `Data.List` for lists. Purescript's compiler doesn't have a literal
syntax for lists, so instead of this:

```elm
[1, 2, 3]
```

... you need to do something like this:

```purescript
import Elm.List (List(..), (:))

(1 : 2 : 3 : Nil)
```

... or, indeed, this:

```purescript
import Elm.List (List(..))

(Cons 1 (Cons 2 (Cons 3 Nil)))
```

... or, there would be a way to do it with `do` notation.


### Array

There is a literal syntax for Array, e.g. `[1, 2, 3]`. However, the `Array` type in Purescript
is actually a Javascript array, which is typically not what you want (unless you're getting
one from elsewhere anyway). And, it's not what `Elm.Array` is.

What you can do, though, to get a list is something like this:

```purescript
Data.List.toList [1, 2, 3]
```

... which is a nice little trick when porting code, as all you have to add is the `Data.List.toList`.
