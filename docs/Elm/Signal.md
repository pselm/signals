## Module Elm.Signal

#### `setup`

``` purescript
setup :: forall e m a. (MonadEff (ref :: REF, now :: Now | e) m) => GraphState m a -> m a
```

#### `Signal`

``` purescript
newtype Signal a
```

#### `constant`

``` purescript
constant :: forall e m a. (MonadEff (ref :: REF | e) m) => a -> GraphState m (Signal a)
```

#### `foldp`

``` purescript
foldp :: forall e m a s. (MonadEff (ref :: REF | e) m) => (a -> s -> s) -> s -> Signal a -> GraphState m (Signal s)
```

#### `filter`

``` purescript
filter :: forall e m a. (MonadEff (ref :: REF | e) m) => (a -> Boolean) -> a -> Signal a -> GraphState m (Signal a)
```

#### `filterMap`

``` purescript
filterMap :: forall e m a b. (MonadEff (ref :: REF | e) m) => (a -> Maybe b) -> b -> Signal a -> GraphState m (Signal b)
```

#### `map`

``` purescript
map :: forall e m a b. (MonadEff (ref :: REF | e) m) => (a -> b) -> Signal a -> GraphState m (Signal b)
```

#### `map2`

``` purescript
map2 :: forall eff m a b r. (MonadEff (ref :: REF | eff) m) => (a -> b -> r) -> Signal a -> Signal b -> GraphState m (Signal r)
```

#### `map3`

``` purescript
map3 :: forall eff m a b c r. (MonadEff (ref :: REF | eff) m) => (a -> b -> c -> r) -> Signal a -> Signal b -> Signal c -> GraphState m (Signal r)
```



#### `map4`

``` purescript
map4 :: forall eff m a b c d r. (MonadEff (ref :: REF | eff) m) => (a -> b -> c -> d -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> GraphState m (Signal r)
```



#### `map5`

``` purescript
map5 :: forall eff m a b c d e r. (MonadEff (ref :: REF | eff) m) => (a -> b -> c -> d -> e -> r) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> GraphState m (Signal r)
```



#### `sampleOn`

``` purescript
sampleOn :: forall e m a b. (MonadEff (ref :: REF | e) m) => Signal a -> Signal b -> GraphState m (Signal b)
```

#### `dropRepeats`

``` purescript
dropRepeats :: forall e m a. (MonadEff (ref :: REF | e) m, Eq a) => Signal a -> GraphState m (Signal a)
```

#### `merge`

``` purescript
merge :: forall e m a. (MonadEff (ref :: REF | e) m) => Signal a -> Signal a -> GraphState m (Signal a)
```

#### `mergeMany`

``` purescript
mergeMany :: forall e m a. (MonadEff (ref :: REF | e) m) => List (Signal a) -> GraphState m (Signal a)
```

#### `Mailbox`

``` purescript
type Mailbox a = { address :: Address a, signal :: Signal a }
```

#### `Address`

``` purescript
newtype Address a
```

#### `mailbox`

``` purescript
mailbox :: forall e m a. (MonadEff (ref :: REF, delay :: DELAY | e) m) => a -> GraphState m (Mailbox a)
```

#### `forwardTo`

``` purescript
forwardTo :: forall a b. Address b -> (a -> b) -> Address a
```

#### `Message`

``` purescript
newtype Message e
```

#### `message`

``` purescript
message :: forall e a. Address a -> a -> Message e
```

#### `send`

``` purescript
send :: forall e a. Address a -> a -> Eff (ref :: REF, now :: Now, delay :: DELAY, console :: CONSOLE | e) Unit
```

#### `current`

``` purescript
current :: forall e a. Signal a -> Eff (ref :: REF | e) a
```

#### `DELAY`

``` purescript
data DELAY :: !
```


