## Module Elm.Mouse

Library for working with mouse input.

#### `position`

``` purescript
position :: forall e m. MonadEff (ref :: REF | e) m => Mouse m (Signal (Tuple Int Int))
```

The current mouse position.

#### `x`

``` purescript
x :: forall e m. MonadEff (ref :: REF | e) m => Mouse m (Signal Int)
```

The current x-coordinate of the mouse.

#### `y`

``` purescript
y :: forall e m. MonadEff (ref :: REF | e) m => Mouse m (Signal Int)
```

The current y-coordinate of the mouse.

#### `isDown`

``` purescript
isDown :: forall e m. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => Mouse m (Signal Bool)
```

The current state of the mouse.
True when any mouse button is down, and false otherwise.

#### `clicks`

``` purescript
clicks :: forall e m. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => Mouse m (Signal Unit)
```

Always equal to unit. Event triggers on every mouse click.

#### `Mouse`

``` purescript
type Mouse m a = ReaderT MouseState (StateT Graph m) a
```

The Mouse API uses some hidden state, managed by this type.

#### `MousePosition`

``` purescript
type MousePosition = { x :: Int, y :: Int }
```

#### `MouseState`

``` purescript
type MouseState = { position :: Signal (Tuple Int Int), node :: EventTarget }
```

#### `setupMouse`

``` purescript
setupMouse :: forall e m a. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => EventTarget -> Mouse m a -> GraphState m a
```

Setup mouse signals, where the mouse is relative to a particular node.

    setupMouse node do
        altSignal <- alt

#### `setupGlobalMouse`

``` purescript
setupGlobalMouse :: forall e m a. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: NOW, console :: CONSOLE | e) m => Mouse m a -> GraphState m a
```

Setup mouse signals, where the mouse is relative to the window.

    setupMouse node do
        altSignal <- alt


