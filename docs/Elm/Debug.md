## Module Elm.Debug

This library is for investigating bugs or performance problems.

#### `log`

``` purescript
log :: forall a. String -> a -> a
```

Log a tagged value on the developer console, and then return the value.

    1 + log "number" 1        -- equals 2, logs "number: 1"
    length (log "start" [])   -- equals 0, logs "start: []"

Notice that `log` is not a pure function! It should *only* be used for
investigating bugs or performance problems.

#### `crash`

``` purescript
crash :: forall a. String -> a
```

Crash the program with an error message.

Equivalent to Purescript's `Partial.Unsafe.unsafeCrashWith`


