## Module Elm.Task

Tasks make it easy to describe asynchronous operations that may fail,
like HTTP requests or writing to a database.

This is implemented on top of Purescript's `Aff` type. The main difference
is that `Task` has a polymorphically-typed error channel, whereas
the error channel for `Aff` can only represent a `String`.

#### `Task`

``` purescript
type Task x a = forall e. ExceptT x (Aff e) a
```

Represents asynchronous effects that may fail. It is useful for stuff like
HTTP.

For example, maybe we have a task with the type (`Task String User`). This means
that when we perform the task, it will either fail with a `String` message or
succeed with a `User`. So this could represent a task that is asking a server
for a certain user.

Implemented in terms of Purescript's `Aff` type, with `ExceptT` layered on top
in order to provide for a polymorphically-typed error channel.

#### `TaskE`

``` purescript
type TaskE e x a = ExceptT x (Aff e) a
```

Equivalent to a `Task`, but with the effect types specified.

#### `toAff`

``` purescript
toAff :: forall eff x a. Task x a -> Aff eff (Either x a)
```

Takes a `Task` and unwraps the underlying `Aff`.

Note that you can use "do notation" directly with the `Task` type -- you
don't have to unwrap it first. Essentially, you only need to unwrap the
`Task` if you need to interact with the `Aff` type.

#### `succeed`

``` purescript
succeed :: forall x a. a -> Task x a
```

A task that succeeds immediately when run.

    succeed 42    -- results in 42

Equivalent to Purescript's `pure`.

#### `fail`

``` purescript
fail :: forall x a. x -> Task x a
```

A task that fails immediately when run.

    fail "file not found" : Task String a

Equivalent to Purescript's `throwError`.

#### `TaskCallback`

``` purescript
type TaskCallback e x a = (x -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit
```

A callback for error and success, to be used when constructing a `Task`
via `makeTask`.

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

Recover from a failure in a task. If the given task fails, we use the
callback to recover.

    fail "file not found" `onError` (\msg -> succeed 42) -- succeed 42
    succeed 9 `onError` (\msg -> succeed 42)             -- succeed 9

Like Purescript's `catchError`, but with a different signature.

#### `mapError`

``` purescript
mapError :: forall x y a. (x -> y) -> Task x a -> Task y a
```

Transform the error value. This can be useful if you need a bunch of error
types to match up.

    type Error = Http Http.Error | WebGL WebGL.Error

    getResources : Task Error Resource
    getResources =
      sequence [ mapError Http serverTask, mapError WebGL textureTask ]

Equivalent to Purescript's `withExceptT`.

#### `toMaybe`

``` purescript
toMaybe :: forall x y a. Task x a -> Task y (Maybe a)
```

Translate a task that can fail into a task that can never fail, by
converting any failure into `Nothing` and any success into `Just` something.

    toMaybe (fail "file not found") -- succeed Nothing
    toMaybe (succeed 42)            -- succeed (Just 42)

This means you can handle the error with the `Maybe` module instead.

#### `fromMaybe`

``` purescript
fromMaybe :: forall x a. x -> Maybe a -> Task x a
```

If you are chaining together a bunch of tasks, it may be useful to treat
a maybe value like a task.

    fromMaybe "file not found" Nothing   -- fail "file not found"
    fromMaybe "file not found" (Just 42) -- succeed 42

#### `toResult`

``` purescript
toResult :: forall x y a. Task x a -> Task y (Result x a)
```

Translate a task that can fail into a task that can never fail, by
converting any failure into `Err` something and any success into `Ok` something.

    toResult (fail "file not found") -- succeed (Err "file not found")
    toResult (succeed 42)            -- succeed (Ok 42)

This means you can handle the error with the `Result` module instead.

#### `fromResult`

``` purescript
fromResult :: forall x a. Result x a -> Task x a
```

If you are chaining together a bunch of tasks, it may be useful to treat
a result like a task.

    fromResult (Err "file not found") -- fail "file not found"
    fromResult (Ok 42)                -- succeed 42

#### `ThreadID`

``` purescript
newtype ThreadID
```

Abstract type that uniquely identifies a thread.

#### `spawn`

``` purescript
spawn :: forall x y a. Task x a -> Task y ThreadID
```

Run a task on a separate thread. This lets you start working with basic
concurrency. In the following example, `task1` and `task2` will be interleaved.
If `task1` makes a long HTTP request, we can hop over to `task2` and do some
work there.

    spawn task1 `andThen` \_ -> task2

#### `sleep`

``` purescript
sleep :: forall x. Time -> Task x Unit
```

Make a thread sleep for a certain amount of time. The following example
sleeps for 1 second and then succeeds with 42.

    sleep 1000 `andThen` \_ -> succeed 42


### Re-exported from Data.Traversable:

#### `sequence`

``` purescript
sequence :: forall a m t. (Traversable t, Applicative m) => t (m a) -> m (t a)
```

### Re-exported from Elm.Apply:

#### `andMap`

``` purescript
andMap :: forall a b f. Apply f => f (a -> b) -> f a -> f b
```

Map a function in a container to a value in a container.

This is the equivalent of Purescript's `apply`.

#### `map2`

``` purescript
map2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `map3`

``` purescript
map3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map4`

``` purescript
map4 :: forall a b c d e f. Apply f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map5`

``` purescript
map5 :: forall a b c d e f g. Apply f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. Bind m => m a -> (a -> m b) -> m b
```

Given some computation, chain its result with another computation.

Equivalent to Purescript's `bind`.

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

