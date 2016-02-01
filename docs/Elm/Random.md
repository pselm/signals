## Module Elm.Random

#### `bool`

``` purescript
bool :: Generator Boolean
```

#### `int`

``` purescript
int :: forall a. (Ord a, Int53Value a) => a -> a -> Generator a
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
initialSeed :: forall a. (Int53Value a) => a -> Seed
```


