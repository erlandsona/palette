module Palette.Generative exposing
    ( complementary, triadic, splitComplementary, square, tetratic
    , monochromatic
    )

{-|


### Generate a palette by hue

@docs complementary, triadic, splitComplementary, square, tetratic


### Generate a palette by lightness

@docs monochromatic

-}

import SolidColor exposing (SolidColor)


{-| Find the color opposite the color you pass in on the color wheel.

E.g., if you pass in a reddish color, you should expect to get back a tealish color.

[Ellie example](https://ellie-app.com/9jNWHMFD6X3a1)

-}
complementary : SolidColor -> SolidColor
complementary color =
    SolidColor.rotateHue 180 color


{-| Find the other two colors in the triadic scheme defined by the color passed in.

Triadic color schemes are evenly-spaced, so each of the three colors is 120 degrees
from the others.

The internet says this scheme will be vibrant, and that you should
mostly use one of the three colors and only use the other two for accents.

[Ellie example](https://ellie-app.com/9jNW6cpLkfba1)

-}
triadic : SolidColor -> ( SolidColor, SolidColor )
triadic color =
    splitComplementary 120 color


{-| Build a three-color scheme by rotating the same amount from the initial color
in both directions.

`triadic`, the evenly-spaced 3-color scheme, can be defined in terms of this function:

    triadic color =
        splitComplementary 120 color

Initial rotation is clamped between 0 and 180.

[Ellie example](https://ellie-app.com/9jNXxc85VR8a1)

-}
splitComplementary : Float -> SolidColor -> ( SolidColor, SolidColor )
splitComplementary r color =
    let
        rotation =
            clamp 0 180 r
    in
    ( SolidColor.rotateHue rotation color, SolidColor.rotateHue (0 - rotation) color )


{-| Find four equally-spaced colors along the color wheel starting from the passed-in color.

[Ellie example](https://ellie-app.com/9jNTjGx86wCa1)

-}
square : SolidColor -> ( SolidColor, SolidColor, SolidColor )
square color =
    tetratic 60 color


{-| Find four colors along the color wheel starting from the passed-in color.

This differs from the `square` helper in that our values aren't equally spaced --
we are selecting colors on the color wheel with a rectangle. We can actually define
`square` in terms of this function as follows:

    square color =
        tetratic 60 color

We'll rotate the number of degrees passed in along the color wheel to find our first
color. Then we'll rotate the "length" of the rectangle -- as much as we need to in order
to make it all the way around.

Initial rotation is clamped between 0 and 180.

[Ellie example](https://ellie-app.com/9jNJP4D3Pgqa1)

-}
tetratic : Float -> SolidColor -> ( SolidColor, SolidColor, SolidColor )
tetratic w color =
    let
        width =
            clamp 0 180 w

        length =
            (360 - 2 * width) / 2
    in
    ( SolidColor.rotateHue width color
    , SolidColor.rotateHue (width + length) color
    , SolidColor.rotateHue (2 * width + length) color
    )


{-| Create a monochromatic palette. The `Float` argument is size of the Lightness
steps that you'd like in the palette.

If you wanted a grayscale palette, and you wanted it to have five colors, you could do
something like this:

    grayscalePalette =
        monochromatic 20 black

Colors will be arranged from darkest to lightest.

[Ellie example](https://ellie-app.com/9jNGn2SvnJda1)

-}
monochromatic : Float -> SolidColor -> List SolidColor
monochromatic stepSize color =
    let
        getNextStep adjustment lastColor colors =
            let
                nextLightness =
                    SolidColor.toHSL lastColor
                        |> (\( _, _, lightness ) -> lightness + adjustment)
            in
            if nextLightness <= 0 || nextLightness >= 100 then
                lastColor :: colors

            else
                getNextStep adjustment (SolidColor.addLightness adjustment lastColor) (lastColor :: colors)
    in
    case List.reverse (getNextStep stepSize color []) of
        start :: tints ->
            getNextStep (0 - stepSize) color [] ++ tints

        [] ->
            []
