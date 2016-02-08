## Module Elm.Regex

A library for working with regular expressions. It uses the same kind of
regular expressions
[accepted by JavaScript](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions).

Purescript has its own regular expression library in `Data.String.Regex`.
However, the `Elm.Regex` API is sufficiently different that it is
re-implemented here.

#### `Regex`

``` purescript
data Regex :: *
```

A regular expression, describing a certain set of strings.

Note that this is (unfortunatley) not interchangeable with Purescript's `Data.String.Regex`,
because the Elm code requires that the `Regex` be created with the `g` flag.

##### Instances
``` purescript
Show Regex
```

#### `escape`

``` purescript
escape :: String -> String
```

Escape strings to be regular expressions, making all special characters
safe. So `regex (escape "^a+")` will match exactly `"^a+"` instead of a series
of `a`&rsquo;s that start at the beginning of the line.

#### `regex`

``` purescript
regex :: String -> Regex
```

Create a Regex that matches patterns
[as specified in JavaScript](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions#Writing_a_Regular_Expression_Pattern).

Be careful to escape backslashes properly! For example, `"\w"` is escaping the
letter `w` which is probably not what you want. You probably want `"\\w"`
instead, which escapes the backslash.

#### `caseInsensitive`

``` purescript
caseInsensitive :: Regex -> Regex
```

Make a regex case insensitive.

#### `contains`

``` purescript
contains :: Regex -> String -> Boolean
```

Check to see if a Regex is contained in a string.

    contains (regex "123") "12345" == True
    contains (regex "b+") "aabbcc" == True

    contains (regex "789") "12345" == False
    contains (regex "z+") "aabbcc" == False

#### `Match`

``` purescript
type Match = { match :: String, submatches :: List (Maybe String), index :: Int, number :: Int }
```

A `Match` represents all of the details about a particular match in a string.
Here are details on each field:

  * `match` &mdash; the full string of the match.

  * `submatches` &mdash; a regex might have [subpatterns, surrounded by
    parentheses](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions#Using_Parenthesized_Substring_Matches).
    If there are N subpatterns, there will be N elements in the `submatches` list.
    Each submatch in this list is a `Maybe` because not all subpatterns may trigger.
    For example, `(regex "(a+)|(b+)")` will either match many `a`&rsquo;s or
    many `b`&rsquo;s, but never both.

  * `index` &mdash; the index of the match in the original string.

  * `number` &mdash; if you find many matches, you can think of each one
    as being labeled with a `number` starting at one. So the first time you
    find a match, that is match `number` one. Second time is match `number` two.
    This is useful when paired with `replace All` if replacement is dependent on how
    many times a pattern has appeared before.

#### `HowMany`

``` purescript
data HowMany
  = All
  | AtMost Int
```

`HowMany` is used to specify how many matches you want to make. So
`replace All` would replace every match, but `replace (AtMost 2)` would
replace at most two matches (i.e. zero, one, two, but never three or more).

#### `find`

``` purescript
find :: HowMany -> Regex -> String -> List Match
```

Find matches in a string:

    findTwoCommas = find (AtMost 2) (regex ",")

      -- map .index (findTwoCommas "a,b,c,d,e") == [1,3]
      -- map .index (findTwoCommas "a b c d e") == []

    places = find All (regex "[oi]n a (\\w+)") "I am on a boat in a lake."

      -- map .match places == ["on a boat", "in a lake"]
      -- map .submatches places == [ [Just "boat"], [Just "lake"] ]

#### `replace`

``` purescript
replace :: HowMany -> Regex -> (Match -> String) -> String -> String
```

Replace matches. The function from `Match` to `String` lets
you use the details of a specific match when making replacements.

    devowel = replace All (regex "[aeiou]") (\_ -> "")

      -- devowel "The quick brown fox" == "Th qck brwn fx"

    reverseWords = replace All (regex "\\w+") (\{match} -> String.reverse match)

      -- reverseWords "deliver mined parts" == "reviled denim strap"

#### `split`

``` purescript
split :: HowMany -> Regex -> String -> List String
```

Split a string, using the regex as the separator.

    split (AtMost 1) (regex ",") "tom,99,90,85" == ["tom","99,90,85"]

    split All (regex ",") "a,b,c,d" == ["a","b","c","d"]


