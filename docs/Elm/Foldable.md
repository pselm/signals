## Module Elm.Foldable

Elm modules define `foldl` with a different signature than Purescript's.
So, we define that alternative `foldl` here.

#### `foldl`

``` purescript
foldl :: forall a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

Reduce a container from the left.

   foldl (:) [] [1,2,3] == [3,2,1]


