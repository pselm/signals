## Module Elm.String

A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are *not* lists of characters.

This is implemented in terms of Purescript's `String` type, so you can also
use functions from `Data.String`.

#### `isEmpty`

``` purescript
isEmpty :: String -> Bool
```

Determine if a string is empty.

    isEmpty "" == True
    isEmpty "the world" == False

Equivalent to Purescript's `null`.

#### `cons`

``` purescript
cons :: Char -> String -> String
```

Add a character to the beginning of a string.

#### `concat`

``` purescript
concat :: List String -> String
```

Concatenate many strings into one.

    concat ["never","the","less"] == "nevertheless"

Equivalent to Purescript's `mconcat`

#### `map`

``` purescript
map :: (Char -> Char) -> String -> String
```

Transform every character in a string

    map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"

#### `filter`

``` purescript
filter :: (Char -> Bool) -> String -> String
```

Keep only the characters that satisfy the predicate.

    filter isDigit "R2-D2" == "22"

#### `reverse`

``` purescript
reverse :: String -> String
```

Reverse a string.

    reverse "stressed" == "desserts"

#### `foldl`

``` purescript
foldl :: forall b. (Char -> b -> b) -> b -> String -> b
```

Reduce a string from the left.

    foldl cons "" "time" == "emit"

#### `foldr`

``` purescript
foldr :: forall b. (Char -> b -> b) -> b -> String -> b
```

Reduce a string from the right.

    foldr cons "" "time" == "time"

#### `split`

``` purescript
split :: String -> String -> List String
```

Split a string using a given separator.

    split "," "cat,dog,cow"        == ["cat","dog","cow"]
    split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

Use [`Regex.split`](Regex#split) if you need something more flexible.

#### `join`

``` purescript
join :: String -> List String -> String
```

Put many strings together with a given separator.

    join "a" ["H","w","ii","n"]        == "Hawaiian"
    join " " ["cat","dog","cow"]       == "cat dog cow"
    join "/" ["home","evan","Desktop"] == "home/evan/Desktop"

#### `repeat`

``` purescript
repeat :: Int -> String -> String
```

Repeat a string *n* times.

    repeat 3 "ha" == "hahaha"

#### `slice`

``` purescript
slice :: Int -> Int -> String -> String
```

Take a substring given a start and end index. Negative indexes
are taken starting from the *end* of the list.

    slice  7  9 "snakes on a plane!" == "on"
    slice  0  6 "snakes on a plane!" == "snakes"
    slice  0 -7 "snakes on a plane!" == "snakes on a"
    slice -6 -1 "snakes on a plane!" == "plane"

#### `left`

``` purescript
left :: Int -> String -> String
```

Take *n* characters from the left side of a string.

Equivalent to Purescript's `left`.

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

Equivalent to Purescript's `drop`.

#### `dropRight`

``` purescript
dropRight :: Int -> String -> String
```

Drop *n* characters from the right side of a string.

#### `pad`

``` purescript
pad :: Int -> Char -> String -> String
```

Pad a string on both sides until it has a given length.

    pad 5 ' ' "1"   == "  1  "
    pad 5 ' ' "11"  == "  11 "
    pad 5 ' ' "121" == " 121 "

#### `padLeft`

``` purescript
padLeft :: Int -> Char -> String -> String
```

Pad a string on the left until it has a given length.

    padLeft 5 '.' "1"   == "....1"
    padLeft 5 '.' "11"  == "...11"
    padLeft 5 '.' "121" == "..121"

#### `padRight`

``` purescript
padRight :: Int -> Char -> String -> String
```

Pad a string on the right until it has a given length.

    padRight 5 '.' "1"   == "1...."
    padRight 5 '.' "11"  == "11..."
    padRight 5 '.' "121" == "121.."

#### `trimLeft`

``` purescript
trimLeft :: String -> String
```

Get rid of whitespace on the left of a string.

    trimLeft "  hats  \n" == "hats  \n"

#### `trimRight`

``` purescript
trimRight :: String -> String
```

Get rid of whitespace on the right of a string.

    trimRight "  hats  \n" == "  hats"

#### `words`

``` purescript
words :: String -> List String
```

Break a string into words, splitting on chunks of whitespace.

    words "How are \t you? \n Good?" == ["How","are","you?","Good?"]

#### `lines`

``` purescript
lines :: String -> List String
```

Break a string into lines, splitting on newlines.

    lines "How are you?\nGood?" == ["How are you?", "Good?"]

#### `any`

``` purescript
any :: (Char -> Bool) -> String -> Bool
```

Determine whether *any* characters satisfy a predicate.

    any isDigit "90210" == True
    any isDigit "R2-D2" == True
    any isDigit "heart" == False

#### `all`

``` purescript
all :: (Char -> Bool) -> String -> Bool
```

Determine whether *all* characters satisfy a predicate.

    all isDigit "90210" == True
    all isDigit "R2-D2" == False
    all isDigit "heart" == False

#### `startsWith`

``` purescript
startsWith :: String -> String -> Bool
```

See if the second string starts with the first one.

    startsWith "the" "theory" == True
    startsWith "ory" "theory" == False

#### `endsWith`

``` purescript
endsWith :: String -> String -> Bool
```

See if the second string ends with the first one.

    endsWith "the" "theory" == False
    endsWith "ory" "theory" == True

#### `indexes`

``` purescript
indexes :: String -> String -> List Int
```

Get all of the indexes for a substring in another string.

    indexes "i" "Mississippi"   == [1,4,7,10]
    indexes "ss" "Mississippi"  == [2,5]
    indexes "needle" "haystack" == []

#### `indices`

``` purescript
indices :: String -> String -> List Int
```

Alias for `indexes`.

#### `toInt`

``` purescript
toInt :: String -> Result String Int
```

Try to convert a string into an int, failing on improperly formatted strings.

    toInt "123" == Ok 123
    toInt "-42" == Ok -42
    toInt "3.1" == Err "could not convert string '3.1' to an Int"
    toInt "31a" == Err "could not convert string '31a' to an Int"

#### `toFloat`

``` purescript
toFloat :: String -> Result String Float
```

Try to convert a string into a float, failing on improperly formatted strings.

    toFloat "123" == Ok 123.0
    toFloat "-42" == Ok -42.0
    toFloat "3.1" == Ok 3.1
    toFloat "31a" == Err "could not convert string '31a' to a Float"

#### `toList`

``` purescript
toList :: String -> List Char
```

Convert a string to a list of characters.

    toList "abc" == ['a','b','c']

#### `fromList`

``` purescript
fromList :: List Char -> String
```

Convert a list of characters into a String. Can be useful if you
want to create a string primarily by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"


### Re-exported from Data.String:

#### `contains`

``` purescript
contains :: String -> String -> Boolean
```

Checks whether the first string exists in the second string.

#### `fromChar`

``` purescript
fromChar :: Char -> String
```

Returns a string of length `1` containing the given character.

#### `length`

``` purescript
length :: String -> Int
```

Returns the number of characters the string is composed of.

#### `toLower`

``` purescript
toLower :: String -> String
```

Returns the argument converted to lowercase.

#### `toUpper`

``` purescript
toUpper :: String -> String
```

Returns the argument converted to uppercase.

#### `trim`

``` purescript
trim :: String -> String
```

Removes whitespace from the beginning and end of a string, including
[whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).

#### `uncons`

``` purescript
uncons :: String -> Maybe { head :: Char, tail :: String }
```

Returns the first character and the rest of the string,
if the string is not empty.

### Re-exported from Prelude:

#### `append`

``` purescript
append :: forall a. (Semigroup a) => a -> a -> a
```

