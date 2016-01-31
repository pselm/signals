## Module Elm.Task

#### `Task`

``` purescript
type Task x a = forall e. ExceptT x (Aff e) a
```

#### `TaskE`

``` purescript
type TaskE e x a = ExceptT x (Aff e) a
```

#### `toAff`

``` purescript
toAff :: forall eff x a. Task x a -> Aff eff (Either x a)
```

#### `succeed`

``` purescript
succeed :: forall x a. a -> Task x a
```

#### `fail`

``` purescript
fail :: forall x a. x -> Task x a
```

#### `TaskCallback`

``` purescript
type TaskCallback e x a = (x -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit
```

#### `makeTask`

``` purescript
makeTask :: forall e x a. TaskCallback e x a -> TaskE e x a
```

Creates a `Task` from a function that accepts error and success callbacks.
To create a `Task` using the foreign function interface (FFI), you would typically
pass this function to the FFI as a parameter. Then, you can do your computation,
and callback with the first parameter to indicate failure or the second callback
to indicate success.

Note that the return value you get from this function can be used as if it were
a `Task x a` ... it's just a bit tricky to ignore the effects types in this case.

#### `onError`

``` purescript
onError :: forall x y a. Task x a -> (x -> Task y a) -> Task y a
```

#### `mapError`

``` purescript
mapError :: forall x y a. (x -> y) -> Task x a -> Task y a
```

#### `toMaybe`

``` purescript
toMaybe :: forall x y a. Task x a -> Task y (Maybe a)
```

#### `fromMaybe`

``` purescript
fromMaybe :: forall x a. x -> Maybe a -> Task x a
```

#### `toResult`

``` purescript
toResult :: forall x y a. Task x a -> Task y (Result x a)
```

#### `fromResult`

``` purescript
fromResult :: forall x a. Result x a -> Task x a
```

#### `ThreadID`

``` purescript
newtype ThreadID
```

#### `spawn`

``` purescript
spawn :: forall x y a. Task x a -> Task y ThreadID
```

#### `sleep`

``` purescript
sleep :: forall x. Time -> Task x Unit
```


