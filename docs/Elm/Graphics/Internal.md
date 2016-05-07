## Module Elm.Graphics.Internal

Some helper functions used internally by multiple modules.
Not part of the official API, thus subject to change without affecting semver.

#### `setStyle`

``` purescript
setStyle :: forall e. String -> String -> Element -> Eff (dom :: DOM | e) Unit
```

#### `removeStyle`

``` purescript
removeStyle :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit
```

#### `getDimensions`

``` purescript
getDimensions :: forall e. Element -> Eff (dom :: DOM | e) { width :: Number, height :: Number }
```

#### `createNode`

``` purescript
createNode :: forall e. String -> Eff (dom :: DOM | e) Element
```

#### `addTransform`

``` purescript
addTransform :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit
```

#### `removeTransform`

``` purescript
removeTransform :: forall e. Element -> Eff (dom :: DOM | e) Unit
```

#### `measure`

``` purescript
measure :: forall e. Node -> Eff (dom :: DOM | e) { width :: Number, height :: Number }
```


