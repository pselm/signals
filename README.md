# purescript-elm

Elm is a programming language that is similar to Purescript in some respects,
but takes a different approach to a variety of questions.

Having done some Elm programming, I wanted to give Purescript a try. I thought
I would port one of my Elm apps to Purescript, but quickly realized that there
were a variety of little differences between the Elm core libraries and their
Purescript equivalents. One possible approach would have been to modify my app.
However, it seemed to me that it might be more interesting to port the Elm
libraries to Purescript -- at least as a first step. I could then change the app
to use more idiomatic Purescript at my leisure.

So, this is a work in progress to do just that.

Each of the Elm libraries is prefixed by the `Elm.` namespace. So, to modify your
existing Elm code, you would simply add `Elm.` to the imports. And, of course,
you can always import `as` something to maintain any internal fully-qualified
references.

Here are a few notes on issues I ran into when performing the port, and how I
handled them. You may find some of this helpful if you are porting Elm code
to Purescript.

## The Type System

This is a big topic, of course -- I'll just focus on some of the mechanical
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

## Tuples

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
  for records (perhaps I just don't know how), so comparing them isn't as easy
  as it might be.

In doing the conversion, I've sometimes used the `Tuple` type, and sometimes
converted to using records instead. In either case, you'll need to make some
modifications to your own code that uses Tuples.


## Booleans

The Purescript type is `Boolean`, rather than the Elm `Bool`. I've put a type
alias in `Elm.Basics` to cover that.

The Boolean literals are `true` and `false`, rather than Elm's `True` and `False`.


## Records

Purescript records are broadly similar to Elm records, with some differences
in syntax. 

### Initialization

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

### Access and accesor function

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

### Update and updater functions

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

### Punning

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


## Unit

Elm uses a Tuple0 -- that is, `()` -- as a type (and value) when a value is
required but there is no natural value to supply.

Purescript has a `Unit` type for this purpose, which is inhabited by a `unit`
value.


## Numbers

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


## Imports

Purescript doesn't import anything by default. So, if you want something, you
have to import it manually.

A "plain" import statement imports everything in a module. So, in Purescript,
`import Data.List` is equivalent to the Elm `import List(..)`.

If you're going to refer to something in fully-qualified way, it appears that
you don't actually have to import it at all, which is unlike Elm.

To import a type, you need to supply the parentheses ... for instance, `import
Data.List (List())`. If you try it without the parentheses, then Purescript
thinks you're trying to import a type class, rather than a type. Of course, you
can import all or some of the constructors, in the usual way -- i.e.
`import Data.List (..)` or `import Data.List (Cons, Nil)`. Note that this will
change in Purescript 0.8 -- I belive you'll have to use the `class` keyword to
import classes, and you'll import types without the parentheses.

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


## List

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


## Array

There is a literal syntax for Array, e.g. `[1, 2, 3]`. However, the `Array` type in Purescript
is actually a Javascript array, which is typically not what you want (unless you're getting
one from elsewhere anyway). And, it's not what `Elm.Array` is.

However, given that there is a literal syntax, I suppose it might be nice to have an
`Elm.List.fromArray`, to make it easier to port code that uses the literal syntax?

