## Module Elm.Maybe

#### `withDefault`

``` purescript
withDefault :: forall a. a -> Maybe a -> a
```

#### `oneOf`

``` purescript
oneOf :: forall a. List (Maybe a) -> Maybe a
```

#### `map`

``` purescript
map :: forall a b. (a -> b) -> Maybe a -> Maybe b
```

#### `map2`

``` purescript
map2 :: forall a b value. (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
```

#### `map3`

``` purescript
map3 :: forall a b c value. (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
```

#### `map4`

``` purescript
map4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
```

#### `map5`

``` purescript
map5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
```

#### `andThen`

``` purescript
andThen :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
```


