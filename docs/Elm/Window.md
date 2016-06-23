## Module Elm.Window

Provides information about the container that your Elm program lives in.
When you embed Elm in a `<div>` it gives the dimensions of the container, not
the whole window.

#### `WindowCallback`

``` purescript
type WindowCallback m a = ReaderT WindowState (StateT Graph m) a
```

The Window API uses some hidden state, managed by this type.

#### `WindowState`

``` purescript
type WindowState = { dimensions :: Signal (Tuple Int Int), node :: Maybe HTMLElement }
```

#### `setupWindow`

``` purescript
setupWindow :: forall e m a. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => HTMLElement -> WindowCallback m a -> GraphState m a
```

Setup window signals.

#### `setupGlobalWindow`

``` purescript
setupGlobalWindow :: forall e m a. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => WindowCallback m a -> GraphState m a
```

Setup window signals.

#### `dimensions`

``` purescript
dimensions :: forall e m. MonadEff (ref :: REF | e) m => WindowCallback m (Signal (Tuple Int Int))
```

The current width and height of the window (i.e. the area viewable to the
user, not including scroll bars).

#### `width`

``` purescript
width :: forall e m. MonadEff (ref :: REF | e) m => WindowCallback m (Signal Int)
```

The current width of the window.

#### `height`

``` purescript
height :: forall e m. MonadEff (ref :: REF | e) m => WindowCallback m (Signal Int)
```

The current height of the window.


