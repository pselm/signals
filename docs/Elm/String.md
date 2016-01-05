## Module Elm.String

#### `isEmpty`

``` purescript
isEmpty :: String -> Bool
```

#### `cons`

``` purescript
cons :: Char -> String -> String
```

Add a character to the beginning of a string. 

#### `concat`

``` purescript
concat :: List String -> String
```

#### `map`

``` purescript
map :: (Char -> Char) -> String -> String
```

#### `filter`

``` purescript
filter :: (Char -> Bool) -> String -> String
```

#### `reverse`

``` purescript
reverse :: String -> String
```

#### `foldl`

``` purescript
foldl :: forall b. (Char -> b -> b) -> b -> String -> b
```

#### `foldr`

``` purescript
foldr :: forall b. (Char -> b -> b) -> b -> String -> b
```

#### `split`

``` purescript
split :: String -> String -> List String
```

#### `join`

``` purescript
join :: String -> List String -> String
```

#### `repeat`

``` purescript
repeat :: Int -> String -> String
```

#### `slice`

``` purescript
slice :: Int -> Int -> String -> String
```

#### `left`

``` purescript
left :: Int -> String -> String
```

Take *n* characters from the left side of a string. 

#### `right`

``` purescript
right :: Int -> String -> String
```

Take *n* characters from the right side of a string. 

#### `dropLeft`

``` purescript
dropLeft :: Int -> String -> String
```

Drop *n* characters from the left side of a string. 

#### `dropRight`

``` purescript
dropRight :: Int -> String -> String
```

Drop *n* characters from the right side of a string. 

#### `pad`

``` purescript
pad :: Int -> Char -> String -> String
```

#### `padLeft`

``` purescript
padLeft :: Int -> Char -> String -> String
```

#### `padRight`

``` purescript
padRight :: Int -> Char -> String -> String
```

#### `trimLeft`

``` purescript
trimLeft :: String -> String
```

#### `trimRight`

``` purescript
trimRight :: String -> String
```

#### `words`

``` purescript
words :: String -> List String
```

#### `lines`

``` purescript
lines :: String -> List String
```

#### `any`

``` purescript
any :: (Char -> Bool) -> String -> Bool
```

#### `all`

``` purescript
all :: (Char -> Bool) -> String -> Bool
```

#### `startsWith`

``` purescript
startsWith :: String -> String -> Bool
```

#### `endsWith`

``` purescript
endsWith :: String -> String -> Bool
```

#### `indexes`

``` purescript
indexes :: String -> String -> List Int
```

#### `indices`

``` purescript
indices :: String -> String -> List Int
```

Alias for `indexes`. 

#### `toInt`

``` purescript
toInt :: String -> Result String Int
```

#### `toFloat`

``` purescript
toFloat :: String -> Result String Float
```

#### `toList`

``` purescript
toList :: String -> List Char
```

#### `fromList`

``` purescript
fromList :: List Char -> String
```


