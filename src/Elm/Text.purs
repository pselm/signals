
-- | A library for styling and displaying text. While the
-- | `String` library focuses on representing and manipulating
-- | strings of character strings, the `Text` library focuses on how those
-- | strings should look on screen. It lets you make text bold or italic,
-- | set the typeface, set the text size, etc.

module Elm.Text
    ( Text
    , fromString, empty, append, concat, join
    , link, Style, style, defaultStyle, Line(..)
    , typeface, monospace, height, color, bold, italic, line
    , renderHtml, drawCanvas
    ) where


import Elm.Basics (Float, Bool)
import Elm.Color (Color, black, toCss)
import Elm.List (List(..), intersperse)
import Elm.Regex (Regex, regex, HowMany(All), replace) as ER
import Elm.Maybe (Maybe (..))
import Elm.String (repeat)
import Data.String (joinWith, contains, length, split)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (class Foldable, foldr)
import Data.String.Regex (RegexFlags, noFlags, replace, regex)
import Data.Array (catMaybes)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Graphics.Canvas (Context2D, Canvas)
import Graphics.Canvas (setFillStyle, setFont, measureText, scale, save, restore) as Canvas
import Control.Monad.Eff (Eff)
import Control.Monad.ST (newSTRef, readSTRef, writeSTRef, modifySTRef, runST)
import Control.Alt ((<|>))
import Prelude (class Show, class Semigroup, ($), (<>), (==), (>>>), (/), mod, (>), show, map, (+), (-), (>>=), bind, pure, negate)


-- | Represents styled text. It can be rendered with collages or with elements.
data Text
    = Text String
    | Append Text Text
    | Meta Props Text


instance showText :: Show Text where
    show = renderHtml


instance semigroupText :: Semigroup Text where
    append = append


-- Given a function that updates the meta props, either update the existing
-- props, or wrap the text in a new Meta
updateProps :: (Props -> Props) -> Text -> Text
updateProps updater (Meta props text) = Meta (updater props) text
updateProps updater text = Meta (updater defaultProps) text


-- | Styles for lines on text. This allows you to add an underline, an overline,
-- | or a strike out text:
-- |
-- |     line Under   (fromString "underline")
-- |     line Over    (fromString "overline")
-- |     line Through (fromString "strike out")
data Line
    = Under
    | Over
    | Through


-- | Represents all the ways you can style `Text`. If the `typeface` list is
-- | empty or the `height` is `Nothing`, the users will fall back on their browser's
-- | default settings. The following `Style` is black, 16 pixel tall, underlined, and
-- | Times New Roman (assuming that typeface is available on the user's computer):
-- |
-- |     { typeface = [ "Times New Roman", "serif" ]
-- |     , height   = Just 16
-- |     , color    = black
-- |     , bold     = false
-- |     , italic   = false
-- |     , line     = Just Under
-- |     }
type Style =
    { typeface :: List String
    , height :: Maybe Float
    , color :: Color
    , bold :: Bool
    , italic :: Bool
    , line :: Maybe Line
    }


-- | Plain black text. It uses the browsers default typeface and text height.
-- | No decorations are used.
-- |
-- |     { typeface = []
-- |     , height = Nothing
-- |     , color = black
-- |     , bold = False
-- |     , italic = False
-- |     , line = Nothing
-- |     }
defaultStyle :: Style
defaultStyle =
    { typeface: Nil
    , height: Nothing
    , color: black
    , bold: false
    , italic: false
    , line: Nothing
    }


-- A type for various meta characteristics that could be set on text.
-- Like `Style`, but more `Maybe`, and with `href`.
type Props =
    { typeface :: List String
    , height :: Maybe Float
    , color :: Maybe Color
    , bold :: Maybe Boolean
    , italic :: Maybe Boolean
    , line :: Maybe Line
    , href :: Maybe String
    }


defaultProps :: Props
defaultProps =
    { typeface: Nil
    , color: Nothing
    , height: Nothing
    , bold: Nothing
    , italic: Nothing
    , line: Nothing
    , href: Nothing
    }


-- | Convert a string into text which can be styled and displayed. To show the
-- | string `"Hello World!"` on screen in italics, you could say:
-- |
-- |     italic (fromString "Hello World!")
fromString :: String -> Text
fromString = Text


-- | Text with nothing in it.
-- |
-- |     empty = fromString ""
empty :: Text
empty = fromString ""


-- | Put two chunks of text together.
-- |
-- |     append (fromString "hello ") (fromString "world") == fromString "hello world"
append :: Text -> Text -> Text
append = Append


-- | Put many chunks of text together.
-- |
-- |     concat
-- |       [ fromString "type "
-- |       , bold (fromString "Maybe")
-- |       , fromString " = Just a | Nothing"
-- |       ]
concat :: ∀ f. (Foldable f) => f Text -> Text
concat texts = foldr append empty texts


-- | Put many chunks of text together with a separator.
-- |
-- |     chunks :: List Text
-- |     chunks = map fromString ("lions" : "tigers" : "bears" : Nil)
-- |
-- |     join (fromString ", ") chunks == fromString "lions, tigers, bears"
join :: Text -> List Text -> Text
join seperator texts =
    concat (intersperse seperator texts)


-- | Set the style of some text. For example, if you design a `Style` called
-- | `footerStyle` that is specifically for the bottom of your page, you could apply
-- | it to text like this:
-- |
-- |     style footerStyle (fromString "the old prince / 2007")
-- |
-- | Note that this method will only *add* styles to text ... if some styles
-- | have already been set, this will not remove them.
style :: Style -> Text -> Text
style new =
    updateProps \old ->
        { typeface: if new.typeface == Nil then old.typeface else new.typeface
        , height: new.height <|> old.height
        , color: Just new.color
        , bold: if new.bold then Just true else old.bold
        , italic: if new.italic then Just true else old.italic
        , line: new.line <|> old.line
        , href: old.href
        }


-- | Provide a list of preferred typefaces for some text.
-- |
-- |     ["helvetica","arial","sans-serif"]
-- |
-- | Not every browser has access to the same typefaces, so rendering will use the
-- | first typeface in the list that is found on the user's computer. If there are
-- | no matches, it will use their default typeface. This works the same as the CSS
-- | font-family property.
typeface :: List String -> Text -> Text
typeface list =
    updateProps $ _ { typeface = list }


-- | Switch to a monospace typeface. Good for code snippets.
-- |
-- |     monospace (fromString "foldl (+) 0 [1,2,3]")
monospace :: Text -> Text
monospace =
    typeface (Cons "monospace" Nil)


-- | Create a link by providing a URL and the text of the link.
-- |
-- |     link "http://elm-lang.org" (fromString "Elm Website")
link :: String -> Text -> Text
link href =
    updateProps $ _ { href = Just href }


-- | Set the height of some text.
-- |
-- |     height 40 (fromString "Title")
height :: Float -> Text -> Text
height value =
    updateProps $ _ { height = Just value }


-- | Set the color of some text.
-- |
-- |     color red (fromString "Red")
color :: Color -> Text -> Text
color newColor =
    updateProps $ _ { color = Just newColor }


-- | Make text bold.
-- |
-- |     fromString "sometimes you want " ++ bold (fromString "emphasis")
bold :: Text -> Text
bold =
    updateProps $ _ { bold = Just true }


-- | Make text italic.
-- |
-- |     fromString "make it " ++ italic (fromString "important")
italic :: Text -> Text
italic =
    updateProps $ _ { italic = Just true }


-- | Put lines on text.
-- |
-- |     line Under   (fromString "underlined")
-- |     line Over    (fromString "overlined")
-- |     line Through (fromString "strike out")
line :: Line -> Text -> Text
line newLine =
    updateProps $ _ { line = Just newLine }


-- RENDER

renderHtml :: Text -> String
renderHtml (Text t) = properEscape t
renderHtml (Append t1 t2) = renderHtml t1 <> renderHtml t2
renderHtml (Meta props t) = renderMeta props (renderHtml t)


renderMeta :: Props -> String -> String
renderMeta props string =
    let
        hrefed =
            case props.href of
                Just href ->
                    "<a href=\"" <> href <> "\">" <> string <> "</a>"

                Nothing ->
                    string

        styleString =
            joinWith "" $
                map styleToString $
                    catMaybes
                        [ map colorToCss props.color
                        , typefaceToCss props.typeface
                        , map heightToCss props.height
                        , props.bold >>= boldToCss
                        , props.italic >>= italicToCss
                        , map lineToCss props.line
                        ]

    in
        if styleString == ""
            then hrefed
            else "<span style=\"" <> styleString <> "\">" <> hrefed <> "</span>"



styleToString :: Tuple String String -> String
styleToString tuple =
    fst tuple <> ":" <> snd tuple <> ";"


colorToCss :: Color -> Tuple String String
colorToCss c = Tuple "color" (toCss c)


heightToCss :: Float -> Tuple String String
heightToCss h =
    Tuple "font-size" $
        (show h) <> "px"


lineToCss :: Line -> Tuple String String
lineToCss l =
    Tuple "text-decoration" $
        case l of
            Under -> "underline"
            Over -> "overline"
            Through -> "line-through"


boldToCss :: Bool -> Maybe (Tuple String String)
boldToCss isBold =
    if isBold
        then Just $ Tuple "font-weight" "bold"
        else Nothing


italicToCss :: Bool -> Maybe (Tuple String String)
italicToCss isItalic =
    if isItalic
        then Just $ Tuple "font-style" "italic"
        else Nothing


quoteSpaces :: String -> String
quoteSpaces string =
    if contains " " string
        then "'" <> string <> "'"
        else string


typefaceToCss :: List String -> Maybe (Tuple String String)
typefaceToCss list =
    case list of
        Nil ->
            Nothing

        _ ->
            Just $
                Tuple "font-family" $
                    joinWith "," $
                        map quoteSpaces $
                            Data.List.fromList list


global :: RegexFlags
global = noFlags { global = true }


-- Compose the escapes together
escapes :: String -> String
escapes =
    replace (regex "\"" global) "&#34;" >>>
    replace (regex "'" global) "&#39;" >>>
    replace (regex "<" global) "&#60;" >>>
    replace (regex ">" global) "&#62;"


makeLines :: String -> String
makeLines string =
    joinWith "<br/>" $
        map makeSpaces $
            split "\n" string


{- The original Elm algorithm isn't the easiest thing to follow, but I think it
amounts to this:

1. Turn the string into an array of characters.

2. Look at the first element of the array. If it's a space, make it &nbsp;

3. Iterate backwards through the array. For each element, if it starts with a
   space, and the previous element is exactly a space, then append it to the
   previous element, and make it blank. I'm pretty sure what this amounts to is
   consolidate consecutive spaces at the first space.

4. Iterate backwards through the array again. For each element, check whether the
   element starts with a space and has at least 2 characters. (Given what has come
   before, this basically means an element of at least 2 spaces). If it does, then
   replace the multi-spaces by:

   4a. Turn the multi-spaces into an array.

   4b. Iterate backwards over every other element, starting with the
       second-last element. Replace that element with a &nbsp; Note that you could
       end up with a "normal" space to start with, given the "every other element".
       And you'd definitely end up with a normal space at the end.

   4c. Join the array.

5. Then join the big array.

6. Now, check if the resulting string ends with a space. If so, replace it with
   an &nbsp;

So, what does this actually all amount to? It appears to be a convenient way to
include an &nbsp; in some text. Basically, there are three ways to include an
&nbsp; (though this simplifies the interactions between the three).

1. If the string starts with a space, then that space will be replaced by an &nbsp;

2. If there is a run of spaces in the middle somewhere, then turn consecutive spaces,
   that is, "  ", into "&nbsp; " ... that is, an &nbsp; followed by a space.

3. If the string ends with a space, then that space will be replaced by an &nbsp;

Now, there are a few subtleties to note here.

1. The first step is performed first, so an initial space is definitively made into
   an &nbsp; and doesn't figure in the rest of the algorithm. So:

   "abc" -> "abc"
   " abc" -> "&nbsp;abc"
   "  abc" -> "&nbsp; abc"
   "   abc" -> "&nbsp;&nbsp; abc"
   "    abc" -> "&nbsp; &nbsp; abc"
   "     abc" -> "&nbsp;&nbsp; &nbsp; abc"

2. The second step substitutes counting from the end. So, if there is an odd number
   of spaces, the result will start and end with a space. So:

   "abcde" -> "abcde"
   "ab cde" -> "ab cde"
   "ab  cde" -> "ab&nbsp; cde"
   "ab   cde" -> "ab &nbsp; cde"
   "ab    cde" -> "ab&nbsp; &nbsp; cde"
   "ab     cde" -> "ab &nbsp; &nbsp; cde"

3. The third step occurs last. So, you could end up with consecutive &nbsp; (as at
   the beginning):

   "abc" -> "abc"
   "abc " -> "abc&nbsp;"
   "abc  " -> "abc&nbsp;&nbsp;"
   "abc   " -> "abc &nbsp;&nbsp;"
   "abc    " -> "abc&nbsp; &nbsp;&nbsp;"
   "abc     " -> "abc &nbsp; &nbsp;&nbsp;"

4. And, of course, one needs to be able to handle multiple runs of spaces in the middle:

   "abcdef" -> "abcdef"
   "ab cd ef" -> "ab cd ef"
   "ab  cd  ef" -> "ab&nbsp; cd&nbsp; ef"
   "ab   cd   ef" -> "ab &nbsp; cd &nbsp; ef"
   "ab    cd    ef" -> "ab&nbsp; &nbsp; cd&nbsp; &nbsp; ef"
   "ab     cd     ef" -> "ab &nbsp; &nbsp; cd &nbsp; &nbsp; ef"

In any event, I've run all of those with the original Elm function, and those are indeed
the results you get. So, I can use that as the test-cases. I'm not super-fond of the
original Elm algorithm as such, so will probably implement it a little differently.
-}
makeSpaces :: String -> String
makeSpaces string =
    let
        stringLength =
            length string

    in
        if stringLength == 0
            then string
            else
                let
                    replacer match =
                        let
                            matchLength =
                                length match.match

                        in
                            if match.index == 0
                                then beginningSpaces matchLength
                                else
                                    if match.index + matchLength == stringLength
                                        then endSpaces matchLength
                                        else middleSpaces matchLength

                in
                    ER.replace ER.All oneOrMoreSpaces replacer string


-- Given a number of spaces, return the string to be used if in the middle.
middleSpaces :: Int -> String
middleSpaces run =
    let
        pairs = run / 2
        extra = run `mod` 2

    in
        -- So, basically, "&nbsp; " for each pair of spaces, and add
        -- a space at the beginning if one is left over.
        if run > 0
            then repeat extra " " <> repeat pairs "&nbsp; "
            else ""


-- Given a number of spaces, return the string to be used if at the end.
endSpaces :: Int -> String
endSpaces run =
    let
        pairs = (run - 1) / 2
        extra = (run - 1) `mod` 2

    in
        -- We unconditionally want an &nbsp; at the end ... then, the
        -- rest is " &nbsp;" for each pair, and an "&nbsp;" at the
        -- begining if there's an extra.
        if run > 0
            then repeat extra "&nbsp;" <> repeat pairs " &nbsp;" <> "&nbsp;"
            else ""


-- Given a number of spaces, return the string to be used if at the beginning.
beginningSpaces :: Int -> String
beginningSpaces run =
    -- Basically, we want an unconditional &nbsp; and then whatever middleSpaces
    -- would do with the rest.
    if run > 0
        then "&nbsp;" <> middleSpaces (run - 1)
        else ""


oneOrMoreSpaces :: ER.Regex
oneOrMoreSpaces = ER.regex "\\s+"


properEscape :: String -> String
properEscape string =
    if string == ""
        then string
        else makeLines $ escapes string


-- DRAW

defaultHeight :: Number
defaultHeight = 12.0


drawCanvas :: ∀ e.
    (∀ eff. Context2D -> String -> Number -> Number -> Eff (canvas :: Canvas | eff) Context2D) ->
    Text ->
    Context2D ->
    Eff (canvas :: Canvas | e) Context2D

drawCanvas draw text ctx =
    runST do
        Canvas.save ctx
        Canvas.scale {scaleX: 1.0, scaleY: (-1.0)} ctx

        -- To avoid multiple iterations ...
        maxHeightRef <- newSTRef 0.0
        totalWidthRef <- newSTRef 0.0

        let
            chunks = chunkText defaultProps text

        measured <-
            for chunks \chunk -> do
                let
                    font = toFont chunk.props
                    chunkHeight = fromMaybe defaultHeight chunk.props.height

                Canvas.setFont font ctx

                metrics <-
                    Canvas.measureText ctx chunk.text

                modifySTRef maxHeightRef \h ->
                    if h > chunkHeight then h else chunkHeight

                modifySTRef totalWidthRef \w ->
                    w + metrics.width

                pure
                    { width: metrics.width
                    , height: chunkHeight
                    , chunk
                    , font
                    }

        maxHeight <- readSTRef maxHeightRef
        totalWidth <- readSTRef totalWidthRef

        xRef <- newSTRef $ (-totalWidth) / 2.0

        for measured \m -> do
            Canvas.setFont m.font ctx
            Canvas.setFillStyle (fromMaybe "black" $ map toCss m.chunk.props.color) ctx

            x <- readSTRef xRef
            draw ctx m.chunk.text x (maxHeight / 2.0)
            writeSTRef xRef $ x + m.width

        Canvas.restore ctx
        pure ctx


toFont :: Props -> String
toFont props =
    joinWith " "
        [ italicToFont props.italic
        , "normal" -- font-variant
        , boldToFont props.bold
        , heightToFont props.height
        , typefaceToFont props.typeface
        ]


boldToFont :: Maybe Bool -> String
boldToFont (Just true) = "bold"
boldToFont _ = "normal"


italicToFont :: Maybe Bool -> String
italicToFont (Just true) = "italic"
italicToFont _ = "normal"


heightToFont :: Maybe Number -> String
heightToFont (Just h) = show h <> "px"
heightToFont _ = show defaultHeight <> "px"


typefaceToFont :: List String -> String
typefaceToFont Nil = "san-serif"
typefaceToFont list =
    joinWith "," $
        map quoteSpaces $
            Data.List.fromList list


type Chunk =
    { text :: String
    , props :: Props
    }


-- Convert the object returned by the text module
-- into something we can use for styling canvas text
chunkText :: Props -> Text -> List Chunk
chunkText props text =
    case text of
        Append left right ->
            chunkText props left <> chunkText props right

        Text text ->
            Data.List.singleton { text, props }

        Meta newProps text ->
            chunkText (combineProps props newProps) text


combineProps :: Props -> Props -> Props
combineProps old new =
    { typeface: if new.typeface == Nil then old.typeface else new.typeface
    , height: new.height <|> old.height
    , color: new.color <|> old.color
    , bold: new.bold <|> old.bold
    , italic: new.italic <|> old.italic
    , line: new.line <|> old.line
    , href: new.href <|> old.href
    }

