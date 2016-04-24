## Module Elm.Keyboard

Library for working with keyboard input.

#### `Keyboard`

``` purescript
type Keyboard m a = ReaderT KeyboardState (StateT Graph m) a
```

The Keyboard API uses some hidden state, managed by this type.

#### `KeyboardState`

``` purescript
type KeyboardState = { model :: Signal Model, keysDown :: Signal (Set KeyCode) }
```

#### `setupKeyboard`

``` purescript
setupKeyboard :: forall e m a. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m => Keyboard m a -> GraphState m a
```

Setup keyboard signals.

    setupKeyboard do
        altSignal <- alt

#### `XY`

``` purescript
newtype XY
```

##### Instances
``` purescript
Eq XY
Show XY
```

#### `arrows`

``` purescript
arrows :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal XY)
```

A signal of records indicating which arrow keys are pressed.

* `{ x = 0, y = 0 }` when pressing no arrows.
* `{ x =-1, y = 0 }` when pressing the left arrow.
* `{ x = 1, y = 1 }` when pressing the up and right arrows.
* `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.

#### `wasd`

``` purescript
wasd :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal XY)
```

Just like the arrows signal, but this uses keys w, a, s, and d,
which are common controls for many computer games.

#### `isDown`

``` purescript
isDown :: forall e m. MonadEff (ref :: REF | e) m => KeyCode -> Keyboard m (Signal Bool)
```

Whether an arbitrary key is pressed.

#### `alt`

``` purescript
alt :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

Whether the alt key is pressed.

#### `ctrl`

``` purescript
ctrl :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

Whether the ctrl key is pressed.

#### `meta`

``` purescript
meta :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

Whether the meta key is pressed.

The meta key is the Windows key on Windows and the Command key on Mac.

#### `shift`

``` purescript
shift :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

Whether the shift key is pressed.

#### `space`

``` purescript
space :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

Whether the space key is pressed.

#### `enter`

``` purescript
enter :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal Bool)
```

#### `keysDown`

``` purescript
keysDown :: forall e m. MonadEff (ref :: REF | e) m => Keyboard m (Signal (Set KeyCode))
```

Set of keys that are currently down.

#### `presses`

``` purescript
presses :: forall e m. MonadEff (ref :: REF, delay :: DELAY, dom :: DOM, now :: Now, console :: CONSOLE | e) m => Keyboard m (Signal KeyCode)
```

The latest key that has been pressed.

#### `Model`

``` purescript
type Model = { alt :: Boolean, meta :: Boolean, keyCodes :: Set KeyCode }
```

#### `KeyCode`

``` purescript
type KeyCode = Int
```


