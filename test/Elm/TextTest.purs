module Test.Elm.TextTest (tests) where

import Test.Unit (TestUnit, test)
import Test.Unit.Assert (equal)

import Elm.Text
import Prelude (bind, ($), map)
import Data.List (List(..), (:), toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Foldable (traverse_)
import Color (white)


infixl 9 equal as ===


tests :: ∀ e. TestUnit e
tests =
    test "Elm.Text\n" do
        let
            text =
                fromString "Text"

        test "fromString" do
            equal "bob" $ renderHtml $ fromString "bob"

            equal
                "<span style=\"font-style:italic;\">Hello World!</span>" $
                renderHtml $ italic $ fromString "Hello World!"

        test "empty" do
            equal "" $ renderHtml empty

        test "append" do
            -- Note that this isn't actually identical to `fromString("hello world")`,
            -- because of the way in which `makeSpaces` works ... I should double-check
            -- whether that's true in Elm as well. And, it is ... the &nbsp; below is correct
            -- Elm-ish behaviour.
            equal
                "hello&nbsp;world" $
                renderHtml $
                    append (fromString "hello ") (fromString "world")

        test "concat" do
            equal
                "type&nbsp;<span style=\"font-weight:bold;\">Maybe</span>&nbsp;= Just a | Nothing" $
                renderHtml $
                    concat
                        [ fromString "type "
                        , bold (fromString "Maybe")
                        , fromString " = Just a | Nothing"
                        ]

        test "join" do
            equal
                "lions,&nbsp;tigers,&nbsp;bears" $
                renderHtml $
                    join
                        (fromString ", ")
                        (map fromString ("lions" : "tigers" : "bears" : Nil))

        test "style" do
            -- Note that because style's color field is a Color, not a Maybe Color,
            -- you're always going to end up with *some* explicit color in the style
            -- if you use the `style` method. I guess you can use the individual
            -- methods instead if you want to avoid that.
            equal
                "<span style=\"color:hsl(0.0, 0.0%, 0.0%);\">Text</span>" $
                renderHtml $
                    style defaultStyle text

            let typefaceStyle = defaultStyle { typeface = toList ["Times", "Helvetica"] }

            equal
                "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-family:Times,Helvetica;\">Text</span>" $
                renderHtml $
                    style typefaceStyle text

            let heightStyle = defaultStyle { height = Just 24.0 }

            equal
                "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-size:24.0px;\">Text</span>" $
                renderHtml $
                    style heightStyle text

            let colorStyle = defaultStyle { color = white }

            equal "<span style=\"color:hsl(0.0, 0.0%, 100.0%);\">Text</span>" $
                renderHtml $
                    style colorStyle text

            let boldStyle = defaultStyle { bold = true }

            equal "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-weight:bold;\">Text</span>" $
                renderHtml $
                    style boldStyle text

            let italicStyle = defaultStyle { italic = true }

            equal "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-style:italic;\">Text</span>" $
                renderHtml $
                    style italicStyle text

            let lineStyle = defaultStyle { line = Just Under }

            equal "<span style=\"color:hsl(0.0, 0.0%, 0.0%);text-decoration:underline;\">Text</span>" $
                renderHtml $
                    style lineStyle text

            let multiStyle = defaultStyle { italic = true, bold = true }

            equal "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-weight:bold;font-style:italic;\">Text</span>" $
                renderHtml $
                    style multiStyle text

            -- Like multiStyle, but sequential. Note that you can't use `style` to turn
            -- something off once it's on ... e.g. the italicStyle has bold: false, but
            -- that doesn't turn it off if it's already on. I've verified that this is
            -- also the behaviour in Elm.
            equal "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-weight:bold;font-style:italic;\">Text</span>" $
                renderHtml $
                    style italicStyle $
                        style boldStyle $
                            text

        test "typefaceToCss" do
            let
                typefaceStyle =
                    defaultStyle { typeface = toList ["Times New Roman", "Helvetica"] }

            equal
                "<span style=\"color:hsl(0.0, 0.0%, 0.0%);font-family:'Times New Roman',Helvetica;\">Text</span>" $
                renderHtml $
                    style typefaceStyle text

        test "typeface" do
            equal
                "<span style=\"font-family:Helvetica,arial,san-serif;\">Text</span>" $
                renderHtml $
                    typeface ("Helvetica" : "arial" : "san-serif" : Nil) text

        test "monospace" do
            equal
                "<span style=\"font-family:monospace;\">Text</span>" $
                renderHtml $
                    monospace text

        test "link" do
            equal
                "<a href=\"http://elm-lang.org\">Elm Website</a>" $
                renderHtml $
                    link "http://elm-lang.org" (fromString "Elm Website")

        test "height" do
            equal
                "<span style=\"font-size:32.0px;\">Text</span>" $
                renderHtml $
                    height 32.0 text

        test "color" do
            equal
                "<span style=\"color:hsl(0.0, 0.0%, 100.0%);\">Text</span>" $
                renderHtml $
                    color white text

        test "bold" do
            equal
                "sometimes you want&nbsp;<span style=\"font-weight:bold;\">emphasis</span>" $
                renderHtml $
                    append
                        (fromString "sometimes you want ")
                        (bold (fromString "emphasis"))

        test "italic" do
            equal
                "make it&nbsp;<span style=\"font-style:italic;\">important</span>" $
                renderHtml $
                    append
                        (fromString "make it ")
                        (italic (fromString "important"))

        test "line" do
            equal
                "<span style=\"text-decoration:underline;\">underlined</span>" $
                renderHtml $
                    line Under (fromString "underlined")

            equal
                "<span style=\"text-decoration:overline;\">overlined</span>" $
                renderHtml $
                    line Over (fromString "overlined")

            equal
                "<span style=\"text-decoration:line-through;\">strike out</span>" $
                renderHtml $
                    line Through (fromString "strike out")

        test "href & style" do
            equal
                "<span style=\"font-weight:bold;\"><a href=\"http://www.apple.com/\">Text</a></span>" $
                renderHtml $
                    bold $
                        link "http://www.apple.com/" $
                            text

        test "escapes" do
            equal
                "&#34;&#39;&#60;&#62;" $
                renderHtml $
                    fromString "\"'<>"

        test "lines" do
            equal
                "Line 1<br/>Line 2" $
                renderHtml $
                    fromString "Line 1\nLine 2"

        test "makeSpaces" do
            let
                check stimulus expected =
                    equal
                        expected $
                        renderHtml $
                            fromString stimulus

            traverse_ (uncurry check)
                ( Tuple "abc" "abc"
                : Tuple " abc" "&nbsp;abc"
                : Tuple "  abc" "&nbsp; abc"
                : Tuple "   abc" "&nbsp;&nbsp; abc"
                : Tuple "    abc" "&nbsp; &nbsp; abc"
                : Tuple "     abc" "&nbsp;&nbsp; &nbsp; abc"

                : Tuple "abcde" "abcde"
                : Tuple "ab cde" "ab cde"
                : Tuple "ab  cde" "ab&nbsp; cde"
                : Tuple "ab   cde" "ab &nbsp; cde"
                : Tuple "ab    cde" "ab&nbsp; &nbsp; cde"
                : Tuple "ab     cde" "ab &nbsp; &nbsp; cde"

                : Tuple "abc" "abc"
                : Tuple "abc " "abc&nbsp;"
                : Tuple "abc  " "abc&nbsp;&nbsp;"
                : Tuple "abc   " "abc &nbsp;&nbsp;"
                : Tuple "abc    " "abc&nbsp; &nbsp;&nbsp;"
                : Tuple "abc     " "abc &nbsp; &nbsp;&nbsp;"

                : Tuple "abcdef" "abcdef"
                : Tuple "ab cd ef" "ab cd ef"
                : Tuple "ab  cd  ef" "ab&nbsp; cd&nbsp; ef"
                : Tuple "ab   cd   ef" "ab &nbsp; cd &nbsp; ef"
                : Tuple "ab    cd    ef" "ab&nbsp; &nbsp; cd&nbsp; &nbsp; ef"
                : Tuple "ab     cd     ef" "ab &nbsp; &nbsp; cd &nbsp; &nbsp; ef"

                : Nil
                )

