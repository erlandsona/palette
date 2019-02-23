module Color exposing
    ( Color
    , fromHSL, toHSL, toHSLString
    , fromHSLA, toHSLA, toHSLAString
    , fromRGB, toRGB, toRGBString
    , fromRGBA, toRGBA, toRGBAString
    , fromHexString, toHexString
    , equals
    , luminance
    )

{-|

@docs Color


## HSL values

HSL is short for hue, saturation, and lightness (or luminosity, or random other
L words depending on who you ask. Think "brightness", and you'll be on the right track).

You may be intuitively familiar with HSL color modeling if you've worked with a
color wheel before. It also may be a great place to start working with color if
you enjoyed playing with unit circles and polar coordinates in trigonometry.

![Representation of HSL values on a cylinder](https://upload.wikimedia.org/wikipedia/commons/thumb/6/6b/HSL_color_solid_cylinder_saturation_gray.png/320px-HSL_color_solid_cylinder_saturation_gray.png)
(Image can be seen in context on the [HSL and HSV arcticle on Wikipedia](https://en.wikipedia.org/wiki/HSL_and_HSV). By HSL\_color\_solid\_cylinder.png: SharkDderivative work: SharkD  Talk - HSL\_color\_solid\_cylinder.png, CC BY-SA 3.0, <https://commons.wikimedia.org/w/index.php?curid=9801661>)

HSL models **hue** as a value on a circle. We can pick a hue by providing a degree.
We start at red -- meaning that we can get to red by saying that our hue is 0 degrees or
by saying that our hue is at 360 degrees. Green is at 90, teal is at 180, and
there's a lovely purple at 270.

**Saturation** is how much of the hue is present. When you see a hue of 0 degrees,
a saturation of 100%, and lightness of 50%, your reaction is going to be "Schnickeys! that's red!"
If you change the saturation to 0%, you'll see gray.

**Lightness** is brightness -- 100% is white and 0% is black.

@docs fromHSL, toHSL, toHSLString
@docs fromHSLA, toHSLA, toHSLAString


### RGB values

RGB is short for red-green-blue. This representation of color specifies how much
red, green, and blue are in the color.

I found [this chart](https://en.wikipedia.org/wiki/HSL_and_HSV#/media/File:HSV-RGB-comparison.svg) really
helpful when thinking about how RGB colors work -- it shows red, green, and blue values as piecewise functions
against Hue values. The chart is actually aimed at describing the HSV color space, which is a little
different than the HSL color space, but it may be helpful for your brain too.

As you work with RGB colors, it may also be helpful to know that this color space is **additive**.

This means that if you add red, green, and blue together, you'll end up with white. The more
colors you add, the brighter the result.

This is different than what you may remember from painting in elementary school.
Paint, where you're mixing pigments together, is a **subtractive**
color space. Printing (CMYK color space) is also subtractive.

@docs fromRGB, toRGB, toRGBString
@docs fromRGBA, toRGBA, toRGBAString


## Hex values

Hexadecimal colors actually use the same color space as RGB colors. The difference
between the two systems is in the base: RGB colors are base 10 and hex colors are base 16.

You will need to use hex colors if you're working with an
[HTML input of type color](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color).

@docs fromHexString, toHexString


## Equality

@docs equals


## Color properties

@docs luminance

-}

import Dict
import Internal.Color
import Opacity exposing (Opacity)


{-| -}
type Color
    = Color Internal.Color.Color Opacity


opaqueColor : Internal.Color.Color -> Color
opaqueColor color =
    Color color Opacity.opaque


{-| Build a new color based on HSL values.

    import Color exposing (Color)

    red : Color
    red =
        Color.fromHSL ( 0, 100, 50 )

The hue is specified in degrees, and uses modular arithmetic such that whether you
pass in `0`, `360`, or `-360`, you'll still end up with a red hue.

Saturation is a percentage value. It's clamped between 0 and 100 (inclusive).
Lightness is a percentage value. It's clamped between 0 and 100 (inclusive).

-}
fromHSL : ( Float, Float, Float ) -> Color
fromHSL =
    Internal.Color.fromHSL >> opaqueColor


fromHSLA : { hue : Float, saturation : Float, lightness : Float, alpha : Opacity } -> Color
fromHSLA { hue, saturation, lightness, alpha } =
    Color (Internal.Color.fromHSL ( hue, saturation, lightness )) alpha


{-| Extract the hue, saturation, and lightness values from an existing Color.
-}
toHSL : Color -> ( Float, Float, Float )
toHSL (Color color _) =
    Internal.Color.toHSL color


{-| Extract the hue, saturation, lightness, and alpha values from an existing Color.
-}
toHSLA : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Opacity }
toHSLA (Color color opacity) =
    let
        ( hue, saturation, lightness ) =
            Internal.Color.toHSL color
    in
    { hue = hue, saturation = saturation, lightness = lightness, alpha = opacity }


{-| Get the HSL representation of a color as a `String`.

    import Color exposing (toHSLString)
    import Html exposing (p, text)
    import Html.Attributes exposing (style)
    import Palette.X11 exposing (red)

    view =
        p [ style "color" (toHSLString red) ]
            [ text "Wow! This sure looks red!" ]

-}
toHSLString : Color -> String
toHSLString color =
    let
        ( h, s, l ) =
            toHSL color
    in
    "hsl("
        ++ String.fromFloat h
        ++ ","
        ++ String.fromFloat s
        ++ "%,"
        ++ String.fromFloat l
        ++ "%)"


{-| -}
toHSLAString : Color -> String
toHSLAString (Color color opacity) =
    let
        ( h, s, l ) =
            Internal.Color.toHSL color
    in
    "hsla("
        ++ String.fromFloat h
        ++ ","
        ++ String.fromFloat s
        ++ "%,"
        ++ String.fromFloat l
        ++ "%,"
        ++ Opacity.toString opacity
        ++ ")"


{-| Build a new color based on RGB values.

    import Color exposing (Color)

    red : Color
    red =
        Color.fromRGB ( 255, 0, 0 )

    green : Color
    green =
        Color.fromRGB ( 0, 255, 0 )

    blue : Color
    blue =
        Color.fromRGB ( 0, 0, 255 )

This function clamps each rgb value between 0 and 255 (inclusive).

-}
fromRGB : ( Float, Float, Float ) -> Color
fromRGB =
    Internal.Color.fromRGB >> opaqueColor


{-| -}
fromRGBA : { red : Float, green : Float, blue : Float, alpha : Opacity } -> Color
fromRGBA { red, green, blue, alpha } =
    Color (Internal.Color.fromRGB ( red, green, blue )) alpha


{-| Extract the red, green, blue values from an existing Color.
-}
toRGB : Color -> ( Float, Float, Float )
toRGB (Color color _) =
    Internal.Color.toRGB color


{-| Extract the red, green, blue, and alpha values from an existing Color.
-}
toRGBA : Color -> { red : Float, green : Float, blue : Float, alpha : Opacity }
toRGBA (Color color opacity) =
    let
        ( red, green, blue ) =
            Internal.Color.toRGB color
    in
    { red = red, green = green, blue = blue, alpha = opacity }


{-| Get the RGB representation of a color as a `String`.

    import Color exposing (toRGBString)
    import Html exposing (p, text)
    import Html.Attributes exposing (style)
    import Palette.X11 exposing (red)

    view =
        p [ style "color" (toRGBString red) ]
            [ text "Wow! This sure looks red!" ]

-}
toRGBString : Color -> String
toRGBString color =
    let
        ( r, g, b ) =
            toRGB color
    in
    "rgb("
        ++ String.fromFloat r
        ++ ","
        ++ String.fromFloat g
        ++ ","
        ++ String.fromFloat b
        ++ ")"


{-| -}
toRGBAString : Color -> String
toRGBAString (Color color opacity) =
    let
        ( r, g, b ) =
            Internal.Color.toRGB color
    in
    "rgba("
        ++ String.fromFloat r
        ++ ","
        ++ String.fromFloat g
        ++ ","
        ++ String.fromFloat b
        ++ ","
        ++ Opacity.toString opacity
        ++ ")"


{-| Build a new color from a hex string. Supports lowercase or uppercase strings.

    (Color.fromHexString "#FFDD00" == Color.fromHexString "#FD0")
        && (Color.fromHexString "#FFDD00" == Color.fromHexString "#ffdd00")

-}
fromHexString : String -> Result String Color
fromHexString colorString =
    case Internal.Color.fromHexString colorString of
        Just color ->
            Ok (opaqueColor color)

        Nothing ->
            Err ("fromHexString could not convert " ++ colorString ++ " to a Color.")


{-| Get the Hex representation of a color as a `String`.

    import Color exposing (toHexString)
    import Html exposing (p, text)
    import Html.Attributes exposing (type_, value)
    import Palette.X11 exposing (red)

    view =
        Html.input
            [ type_ "color"
            , value (toHexString red)
            ]
            []

Note: this function will always return a string in the form "#RRGGBB". It will
not return shortened values (e.g., "#RGB").

If you want or need this functionality, please make an issue for it on the
github repo for this library.

-}
toHexString : Color -> String
toHexString (Color color _) =
    let
        ( r, g, b ) =
            Internal.Color.toHex color
    in
    "#" ++ r ++ g ++ b


{-| Check two colors for equality.
-}
equals : Color -> Color -> Bool
equals a b =
    toRGBA a == toRGBA b


{-| Luminance calculation adopted from <https://www.w3.org/TR/WCAG20-TECHS/G17.html>

Luminance describes the perceived brightness of a color. You're unlikely to need
to use this function directly. Maybe something in `Color.Contrast` or `Color.Generator`
meets your needs instead?

-}
luminance : Color -> Float
luminance color =
    let
        ( rRaw, gRaw, bRaw ) =
            toRGB color

        red =
            rRaw
                |> toSRBG
                |> fromSRGB

        green =
            gRaw
                |> toSRBG
                |> fromSRGB

        blue =
            bRaw
                |> toSRBG
                |> fromSRGB

        toSRBG rgb8bit =
            rgb8bit / 255

        fromSRGB srgb =
            if srgb <= 0.03928 then
                srgb / 12.92

            else
                ((srgb + 0.055) / 1.055) ^ 2.4
    in
    (0.2126 * red) + (0.7152 * green) + (0.0722 * blue)
