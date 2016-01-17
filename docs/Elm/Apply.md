## Module Elm.Apply

#### `map2`

``` purescript
map2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
```

Lift a function of two arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `map3`

``` purescript
map3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Lift a function of three arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `map4`

``` purescript
map4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```

Lift a function of four arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `map5`

``` purescript
map5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Lift a function of five arguments to a function which accepts and returns
values wrapped with the type constructor `f`.


