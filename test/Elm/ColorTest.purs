module Test.Elm.ColorTest (tests) where

import Test.Unit (TestSuite, Test, suite, test, success, failure)
import Test.Unit.Assert (equal)

import Elm.Color 
import Prelude (flip, bind, class Eq, class Show, show, ($), (-), (<), (<>))
import Elm.Basics (abs)


infixl 9 equals as ===

equals :: ∀ a e. (Eq a, Show a) => a -> a -> Test e
equals = flip equal


infixl 9 close as ~==

close :: ∀ e. Number -> Number -> Test e
close a b =
    if abs (a - b) < 0.00000001
        then success
        else 
            failure $
                "Expected " <> (show b) <> ", got " <> (show a)


tests :: ∀ e. TestSuite e
tests =
    suite "Elm.Color" do
        test "rgb" do
            let
                value =
                    rgb 10 20 30

                rgbResult =
                    toRgb value

                hslResult =
                    toHsl value

            rgbResult.red === 10
            rgbResult.green === 20
            rgbResult.blue === 30
            rgbResult.alpha ~== 1.0

            hslResult.hue ~== 3.66519142918
            hslResult.saturation ~== 0.5
            hslResult.lightness ~== 0.0784313725
            hslResult.alpha ~== 1.0

        test "rgba" do
            let
                value =
                    rgba 10 20 30 0.5

                rgbResult =
                    toRgb value

                hslResult =
                    toHsl value

            rgbResult.red === 10
            rgbResult.blue === 30
            rgbResult.green === 20
            rgbResult.alpha ~== 0.5

            hslResult.hue ~== 3.66519142918
            hslResult.saturation ~== 0.5
            hslResult.lightness ~== 0.0784313725
            hslResult.alpha ~== 0.5

        test "hsl" do
            let
                value =
                    hsl 0.8 0.6 0.4

                rgbResult =
                    toRgb value

                hslResult =
                    toHsl value

            hslResult.hue ~== 0.8
            hslResult.saturation ~== 0.6
            hslResult.lightness ~== 0.4
            hslResult.alpha ~== 1.0

            rgbResult.red === 163
            rgbResult.green === 134
            rgbResult.blue === 41
            rgbResult.alpha ~== 1.0
        
        test "hsla" do
            let
                value =
                    hsla 0.8 0.6 0.4 0.5

                rgbResult =
                    toRgb value

                hslResult =
                    toHsl value

            hslResult.hue ~== 0.8
            hslResult.saturation ~== 0.6
            hslResult.lightness ~== 0.4
            hslResult.alpha ~== 0.5

            rgbResult.red === 163
            rgbResult.green === 134
            rgbResult.blue === 41
            rgbResult.alpha ~== 0.5

        test "greyscale" do
            greyscale 0.5 === rgb 128 128 128
            grayscale 0.5 === rgb 128 128 128

        test "complement" do
            complement (rgb 10 20 30) === rgb 30 20 10

        test "toCss" do
            toCss (hsl 0.8 0.6 0.4) === "hsl(45.84, 60.0%, 40.0%)"
            toCss (hsla 0.8 0.6 0.4 0.2) === "hsla(45.84, 60.0%, 40.0%, 0.2)"

