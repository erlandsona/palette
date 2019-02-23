module Internal.RGB exposing (Channels, Color, fromChannels, fromHSLA, toChannels)


type Color
    = Color Channels


type alias Channels =
    { red : Float, green : Float, blue : Float }


fromChannels : Channels -> Color
fromChannels { red, green, blue } =
    Color
        { red = clamp 0 255 red
        , green = clamp 0 255 green
        , blue = clamp 0 255 blue
        }


toChannels : Color -> Channels
toChannels (Color values) =
    values


fromHSLA : { a | hue : Float, saturation : Float, lightness : Float } -> Color
fromHSLA ({ hue } as hsl) =
    let
        saturation =
            hsl.saturation / 100

        lightness =
            hsl.lightness / 100

        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        hueIsBetween lowerBound upperBound =
            lowerBound <= hue && hue <= upperBound

        zigUp xIntercept =
            chroma * (hue - xIntercept) / 60

        zigDown xIntercept =
            -1 * zigUp xIntercept

        ( r, g, b ) =
            if hueIsBetween 0 60 then
                ( chroma, zigUp 0, 0 )

            else if hueIsBetween 60 120 then
                ( zigDown 120, chroma, 0 )

            else if hueIsBetween 120 180 then
                ( 0, chroma, zigUp 120 )

            else if hueIsBetween 180 240 then
                ( 0, zigDown 240, chroma )

            else if hueIsBetween 240 300 then
                ( zigUp 240, 0, chroma )

            else
                ( chroma, 0, zigDown 360 )

        lightnessModifier =
            lightness - chroma / 2
    in
    fromChannels
        { red = (r + lightnessModifier) * 255
        , green = (g + lightnessModifier) * 255
        , blue = (b + lightnessModifier) * 255
        }
