## Module Elm.Text

A library for styling and displaying text. While the
`String` library focuses on representing and manipulating
strings of character strings, the `Text` library focuses on how those
strings should look on screen. It lets you make text bold or italic,
set the typeface, set the text size, etc.

#### `Text`

``` purescript
data Text
```

Represents styled text. It can be rendered with collages or with elements.

##### Instances
``` purescript
Show Text
Semigroup Text
```

#### `Line`

``` purescript
data Line
  = Under
  | Over
  | Through
```

Styles for lines on text. This allows you to add an underline, an overline,
or a strike out text:

    line Under   (fromString "underline")
    line Over    (fromString "overline")
    line Through (fromString "strike out")

#### `Style`

``` purescript
type Style = { typeface :: List String, height :: Maybe Float, color :: Color, bold :: Bool, italic :: Bool, line :: Maybe Line }
```

Represents all the ways you can style `Text`. If the `typeface` list is
empty or the `height` is `Nothing`, the users will fall back on their browser's
default settings. The following `Style` is black, 16 pixel tall, underlined, and
Times New Roman (assuming that typeface is available on the user's computer):

    { typeface = [ "Times New Roman", "serif" ]
    , height   = Just 16
    , color    = black
    , bold     = false
    , italic   = false
    , line     = Just Under
    }

#### `defaultStyle`

``` purescript
defaultStyle :: Style
```

Plain black text. It uses the browsers default typeface and text height.
No decorations are used.

    { typeface = []
    , height = Nothing
    , color = black
    , bold = False
    , italic = False
    , line = Nothing
    }

#### `fromString`

``` purescript
fromString :: String -> Text
```

Convert a string into text which can be styled and displayed. To show the
string `"Hello World!"` on screen in italics, you could say:

    italic (fromString "Hello World!")

#### `empty`

``` purescript
empty :: Text
```

Text with nothing in it.

    empty = fromString ""

#### `append`

``` purescript
append :: Text -> Text -> Text
```

Put two chunks of text together.

    append (fromString "hello ") (fromString "world") == fromString "hello world"

#### `concat`

``` purescript
concat :: forall f. (Foldable f) => f Text -> Text
```

Put many chunks of text together.

    concat
      [ fromString "type "
      , bold (fromString "Maybe")
      , fromString " = Just a | Nothing"
      ]

#### `join`

``` purescript
join :: Text -> List Text -> Text
```

Put many chunks of text together with a separator.

    chunks :: List Text
    chunks = map fromString ("lions" : "tigers" : "bears" : Nil)

    join (fromString ", ") chunks == fromString "lions, tigers, bears"

#### `style`

``` purescript
style :: Style -> Text -> Text
```

Set the style of some text. For example, if you design a `Style` called
`footerStyle` that is specifically for the bottom of your page, you could apply
it to text like this:

    style footerStyle (fromString "the old prince / 2007")

Note that this method will only *add* styles to text ... if some styles
have already been set, this will not remove them.

#### `typeface`

``` purescript
typeface :: List String -> Text -> Text
```

Provide a list of preferred typefaces for some text.

    ["helvetica","arial","sans-serif"]

Not every browser has access to the same typefaces, so rendering will use the
first typeface in the list that is found on the user's computer. If there are
no matches, it will use their default typeface. This works the same as the CSS
font-family property.

#### `monospace`

``` purescript
monospace :: Text -> Text
```

Switch to a monospace typeface. Good for code snippets.

    monospace (fromString "foldl (+) 0 [1,2,3]")

#### `link`

``` purescript
link :: String -> Text -> Text
```

Create a link by providing a URL and the text of the link.

    link "http://elm-lang.org" (fromString "Elm Website")

#### `height`

``` purescript
height :: Float -> Text -> Text
```

Set the height of some text.

    height 40 (fromString "Title")

#### `color`

``` purescript
color :: Color -> Text -> Text
```

Set the color of some text.

    color red (fromString "Red")

#### `bold`

``` purescript
bold :: Text -> Text
```

Make text bold.

    fromString "sometimes you want " ++ bold (fromString "emphasis")

#### `italic`

``` purescript
italic :: Text -> Text
```

Make text italic.

    fromString "make it " ++ italic (fromString "important")

#### `line`

``` purescript
line :: Line -> Text -> Text
```

Put lines on text.

    line Under   (fromString "underlined")
    line Over    (fromString "overlined")
    line Through (fromString "strike out")

#### `renderHtml`

``` purescript
renderHtml :: Text -> String
```


