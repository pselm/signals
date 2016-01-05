## Module Elm.Char

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

#### `toCode`

``` purescript
toCode :: Char -> KeyCode
```

#### `fromCode`

``` purescript
fromCode :: KeyCode -> Char
```

Convert from unicode. 


