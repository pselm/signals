## Module Elm.Random

#### `bool`

``` purescript
bool :: Generator Boolean
```

#### `int`

``` purescript
int :: forall a. (Ord a, LikeInt53 a) => a -> a -> Generator a
```

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
float :: Number -> Number -> Generator Number
```

#### `pair`

``` purescript
pair :: forall a b. Generator a -> Generator b -> Generator (Tuple a b)
```

#### `list`

``` purescript
list :: forall t a. (Monoid (t a), Applicative t) => Int -> Generator a -> Generator (t a)
```

#### `map`

``` purescript
map :: forall a b. (a -> b) -> Generator a -> Generator b
```

#### `map2`

``` purescript
map2 :: forall a b c. (a -> b -> c) -> Generator a -> Generator b -> Generator c
```

#### `map3`

``` purescript
map3 :: forall a b c d. (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
```

#### `map4`

``` purescript
map4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
```

Combine four generators. 

#### `map5`

``` purescript
map5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
```

Combine five generators. 

#### `andThen`

``` purescript
andThen :: forall a b. Generator a -> (a -> Generator b) -> Generator b
```

#### `Generator`

``` purescript
data Generator a
```

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

#### `generate`

``` purescript
generate :: forall a. Generator a -> Seed -> Generated a
```

#### `initialSeed`

``` purescript
initialSeed :: forall a. (LikeInt53 a) => a -> Seed
```


