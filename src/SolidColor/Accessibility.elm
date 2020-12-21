module SolidColor.Accessibility exposing
    ( Rating(..), meetsAA, meetsAAA
    , checkContrast
    , contrast
    , FontConfig, widen
    )

{-|

@docs Rating, meetsAA, meetsAAA
@docs checkContrast
@docs contrast

[Ellie example](https://ellie-app.com/9jPV682wPLYa1)

-}

import SolidColor exposing (SolidColor)


{-| Read more about levels of conformance at [WCAG](https://www.w3.org/TR/UNDERSTANDING-WCAG20/conformance.html#uc-levels-head).
-}
type Rating
    = Inaccessible
    | AA
    | AAA


type alias FontConfig =
    { size : Int
    , weight : Int
    }


{-| -}
meetsAA : Rating -> Bool
meetsAA rating =
    case rating of
        Inaccessible ->
            False

        AA ->
            True

        AAA ->
            True


{-| -}
meetsAAA : Rating -> Bool
meetsAAA rating =
    case rating of
        Inaccessible ->
            False

        AA ->
            False

        AAA ->
            True


{-| Checks whether two colors have enough contrast with each other to be used together
(e.g., as a background and text color combination). Returns the WCAG Rating level.

To meet AA level sufficiently, [follow these standards](https://www.w3.org/WAI/WCAG21/quickref/?versions=2.0&showtechniques=143%2C146#contrast-minimum).
To meet AAA level sufficiently, [follow these standards](https://www.w3.org/WAI/WCAG21/quickref/?versions=2.0&showtechniques=143%2C146#contrast-enhanced).

-}
checkContrast :
    FontConfig
    -> SolidColor
    -> SolidColor
    -> Rating
checkContrast font color1 color2 =
    if sufficientContrast AAA_ font color1 color2 then
        AAA

    else if sufficientContrast AA_ font color1 color2 then
        AA

    else
        Inaccessible


type WCAGLevel
    = AA_
    | AAA_


{-| -}
sufficientContrast : WCAGLevel -> FontConfig -> SolidColor -> SolidColor -> Bool
sufficientContrast wcagLevel { size, weight } color1 color2 =
    let
        colorContrast =
            contrast color1 color2
    in
    case wcagLevel of
        AA_ ->
            if (size > 14 && weight >= 700) || size > 18 then
                colorContrast >= 3

            else
                colorContrast >= 4.5

        AAA_ ->
            if (size > 14 && weight >= 700) || size > 18 then
                colorContrast >= 4.5

            else
                colorContrast >= 7


{-| Calculate the contrast between two colors.
-}
contrast : SolidColor -> SolidColor -> Float
contrast color1 color2 =
    let
        luminance1 =
            SolidColor.luminance color1

        luminance2 =
            SolidColor.luminance color2
    in
    if luminance1 > luminance2 then
        (luminance1 + 0.05) / (luminance2 + 0.05)

    else
        (luminance2 + 0.05) / (luminance1 + 0.05)


{-| Returns a pair of colors guaranteed to meet Accessibility spec when used together as a fg & bg
-}
widen :
    Rating
    -> FontConfig
    -- Reference
    -> SolidColor
    -- Target
    -> SolidColor
    -- New (Reference, Target)
    -> ( SolidColor, SolidColor )
widen rating fontConf reference target =
    let
        contrastValue =
            checkContrast fontConf reference target

        meetsRating =
            case rating of
                Inaccessible ->
                    True

                AA ->
                    meetsAA contrastValue

                AAA ->
                    meetsAAA contrastValue
    in
    if meetsRating then
        ( reference, target )

    else
        let
            targetLightness : Int
            targetLightness =
                SolidColor.toHSL target |> (\( _, _, a ) -> a) |> round |> clamp 0 100

            mustModifyReference =
                List.member targetLightness [ 100, 0 ]

            newColor ref =
                if SolidColor.luminance ref >= 0.5 then
                    SolidColor.blacken 1

                else
                    SolidColor.whiten 1

            go =
                widen rating fontConf
        in
        if mustModifyReference then
            go (newColor target reference) target

        else
            go reference (newColor reference target)
