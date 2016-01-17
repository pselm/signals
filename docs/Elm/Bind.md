## Module Elm.Bind

#### `andThen`

``` purescript
andThen :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
```

`andThen` is an alias for `bind`.


