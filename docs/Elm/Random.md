## Module Elm.Random

This library helps you generate pseudo-random values.

This library is all about building [`generators`](#Generator) for whatever
type of values you need. There are a bunch of primitive generators like
[`bool`](#bool) and [`int`](#int) that you can build up into fancier
generators with functions like [`list`](#list) and [`map`](#map).

You use a `Generator` by running the [`generate`](#generate) function. If you
need random values across many frames, you will probably want to store the
most recent seed in your application state.

*Note:* This is an implementation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It is almost a direct translation from the
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module. It has a period of roughly 2.30584e18.

This is a translation of the Elm code to Purescript. I suppose the more idiomatic
Purescript way of doing things like this would be to use the `Arbitrary` class in
the purescript-quickcheck library.

#### `bool`

``` purescript
bool :: Generator Bool
```

Create a generator that produces boolean values. The following example
simulates a coin flip that may land heads or tails.

    type Flip = Heads | Tails

    coinFlip : Generator Flip
    coinFlip =
        map (\b -> if b then Heads else Tails) bool

#### `int`

``` purescript
int :: forall a. (Ord a, Int53Value a) => a -> a -> Generator a
```

Generate 32-bit integers in a given range.

    int 0 10   -- an integer between zero and ten
    int -5 5   -- an integer between -5 and 5

    int minInt maxInt  -- an integer in the widest range feasible

Unlike the Elm implementation, this function *cannot* produce values outside of
the range [[`minInt`](#minInt), [`maxInt`](#maxInt)].

#### `maxInt`

``` purescript
maxInt :: Int53
```

The maximum value for randomly generated 32-bit ints.

#### `minInt`

``` purescript
minInt :: Int53
```

The minimum value for randomly generated 32-bit ints.

#### `float`

``` purescript
float :: Float -> Float -> Generator Float
```

Generate floats in a given range. The following example is a generator
that produces decimals between 0 and 1.

    probability : Generator Float
    probability =
        float 0 1

#### `pair`

``` purescript
pair :: forall a b. Generator a -> Generator b -> Generator (Tuple a b)
```

Create a pair of random values. A common use of this might be to generate
a point in a certain 2D space. Imagine we have a collage that is 400 pixels
wide and 200 pixels tall.

    randomPoint : Generator (Int,Int)
    randomPoint =
        pair (int -200 200) (int -100 100)

#### `list`

``` purescript
list :: forall t a. (Monoid (t a), Applicative t) => Int -> Generator a -> Generator (t a)
```

Create a list of random values.

    floatList : Generator (List Float)
    floatList =
        list 10 (float 0 1)

    intList : Generator (List Int)
    intList =
        list 5 (int 0 100)

    intPairs : Generator (List (Int, Int))
    intPairs =
        list 10 <| pair (int 0 100) (int 0 100)

The return type is polymorphic in order to accommodate `List` or `Array`, among others.

#### `Generator`

``` purescript
data Generator a
```

A `Generator` is like a recipe for generating certain random values. So a
`Generator Int` describes how to generate integers and a `Generator String`
describes how to generate strings.

To actually *run* a generator and produce the random values, you need to use
functions like [`generate`](#generate) and [`initialSeed`](#initialSeed).

##### Instances
``` purescript
Functor Generator
Apply Generator
Bind Generator
(Semigroup a) => Semigroup (Generator a)
```

#### `Seed`

``` purescript
data Seed
```

A `Seed` is the source of randomness in this whole system. Whenever
you want to use a generator, you need to pair it with a seed.

#### `generate`

``` purescript
generate :: forall a. Generator a -> Seed -> Generated a
```

Generate a random value as specified by a given `Generator`.

In the following example, we are trying to generate a number between 0 and 100
with the `int 0 100` generator. Each time we call `generate` we need to provide
a seed. This will produce a random number and a *new* seed to use if we want to
run other generators later.

So here it is done right, where we get a new seed from each `generate` call and
thread that through.

    seed0 = initialSeed 31415

    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed1 ==> (31, seed2)
    -- generate (int 0 100) seed2 ==> (99, seed3)

Notice that we use different seeds on each line. This is important! If you use
the same seed, you get the same results.

    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed0 ==> (42, seed1)

#### `initialSeed`

``` purescript
initialSeed :: forall a. (Int53Value a) => a -> Seed
```

Create a &ldquo;seed&rdquo; of randomness which makes it possible to
generate random values. If you use the same seed many times, it will result
in the same thing every time!


### Re-exported from Elm.Apply:

#### `map2`

``` purescript
map2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `map3`

``` purescript
map3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map4`

``` purescript
map4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map5`

``` purescript
map5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
```

Given some computation, chain its result with another computation.

`andThen` is equivalent to Purescript's `bind`.

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. (Functor f) => (a -> b) -> f a -> f b
```

