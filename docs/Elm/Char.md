## Module Elm.Char

Functions for working with characters. Character literals are enclosed in
`'a'` pair of single quotes.

#### `isUpper`

``` purescript
isUpper :: Char -> Bool
```

True for upper case letters.

#### `isLower`

``` purescript
isLower :: Char -> Bool
```

True for lower case letters.

#### `isDigit`

``` purescript
isDigit :: Char -> Bool
```

True for ASCII digits `[0-9]`.

#### `isOctDigit`

``` purescript
isOctDigit :: Char -> Bool
```

True for ASCII octal digits `[0-7]`.

#### `isHexDigit`

``` purescript
isHexDigit :: Char -> Bool
```

True for ASCII hexadecimal digits `[0-9a-fA-F]`.

#### `toLocaleUpper`

``` purescript
toLocaleUpper :: Char -> Char
```

Convert to upper case, according to any locale-specific case mappings.

#### `toLocaleLower`

``` purescript
toLocaleLower :: Char -> Char
```

Convert to lower case, according to any locale-specific case mappings.

#### `KeyCode`

``` purescript
type KeyCode = Int
```

In this library, we use integers to represent the key codes coming from the
keyboard. You can use [`toCode`](#toCode) and [`fromCode`](#fromCode)
to convert between key codes and characters.

#### `toCode`

``` purescript
toCode :: Char -> KeyCode
```

Convert to unicode. Used with the [`Keyboard`](Keyboard) library,
which expects the input to be uppercase.

#### `fromCode`

``` purescript
fromCode :: KeyCode -> Char
```

Convert from unicode.


### Re-exported from Data.Char:

#### `toLower`

``` purescript
toLower :: Char -> Char
```

Converts a character to lowercase.

#### `toUpper`

``` purescript
toUpper :: Char -> Char
```

Converts a character to uppercase.

