module OpaqueColorSpec exposing
    ( colorSpec
    , contrastSuite
    , conversionsSpec
    , luminanceSuite
    , sufficientContrastSuite
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Opacity
import OpaqueColor exposing (OpaqueColor)
import OpaqueColor.Generator
import OpaqueColorFuzzer exposing (hexStringOfLength)
import Palette.X11 exposing (..)
import Test exposing (..)


colorSpec : Test
colorSpec =
    describe "OpaqueColor"
        [ describe "to and from a OpaqueColor"
            [ test "from RGB to RGB" <|
                \_ ->
                    OpaqueColor.fromRGB ( -10, 123, 300 )
                        |> expectRGB ( 0, 123, 255 )
            , test "from HSL to HSL" <|
                \_ ->
                    OpaqueColor.fromHSL ( -10, 123, -10 )
                        |> expectHSL ( 350, 100, 0 )
            , describe "Hex"
                [ test "from Hex with bad values" <|
                    \_ ->
                        OpaqueColor.fromHexString "#FFDG00"
                            |> Expect.err
                , test "from lowercase Hex to Hex" <|
                    \_ ->
                        OpaqueColor.fromHexString "#d3e700"
                            |> expectHex "#D3E700"
                , test "from Hex to Hex" <|
                    \_ ->
                        OpaqueColor.fromHexString "#FFD700"
                            |> expectHex "#FFD700"
                , fuzz (hexStringOfLength 4) "Short hex with transparency" <|
                    \hex ->
                        Expect.ok (OpaqueColor.fromHexString hex)
                , fuzz (hexStringOfLength 8) "Long hex with transparency" <|
                    \hex ->
                        Expect.ok (OpaqueColor.fromHexString hex)
                , fuzz (hexStringOfLength 3) "Short hex and long hex match" <|
                    \hex ->
                        let
                            fullLengthHexString =
                                String.toList hex
                                    |> List.concatMap (\v -> [ v, v ])
                                    |> String.fromList
                                    |> String.dropLeft 1
                        in
                        OpaqueColor.fromHexString hex
                            |> expectHex fullLengthHexString
                , fuzz (hexStringOfLength 6) "Long hex succeeds" <|
                    \hex ->
                        OpaqueColor.fromHexString hex
                            |> expectHex hex
                ]
            ]
        , describe "to a String" <|
            let
                transparentPink =
                    OpaqueColor.fromRGB ( 255, 0, 255 )
            in
            [ test "toRGBString" <|
                \_ ->
                    transparentPink
                        |> OpaqueColor.toRGBString
                        |> Expect.equal "rgb(255,0,255)"
            , test "toHSLString" <|
                \_ ->
                    transparentPink
                        |> OpaqueColor.toHSLString
                        |> Expect.equal "hsl(300,100%,50%)"
            , test "toHexString" <|
                \_ ->
                    transparentPink
                        |> OpaqueColor.toHexString
                        |> Expect.equal "#FF00FF"
            ]
        , describe "equality and equivalence"
            [ test "(==) does not properly compare color values across color spaces" <|
                \_ ->
                    OpaqueColor.fromRGB ( 255, 0, 0 )
                        == OpaqueColor.fromHSL ( 0, 100, 50 )
                        |> Expect.false "(==) compared color values unexpectedly"
            , test "(==) does not properly compare repeated modelings of the same color" <|
                \_ ->
                    -- Both results are black! however (==) won't compare them properly.
                    OpaqueColor.fromHSL ( 3, 50, 0 )
                        == OpaqueColor.fromHSL ( 45, 50, 0 )
                        |> Expect.false "(==) compared color values unexpectedly"
            , describe "equals"
                [ test "when colors are identical, return true" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 100, 50 )
                            |> OpaqueColor.equals (OpaqueColor.fromRGB ( 255, 0, 0 ))
                            |> Expect.true "Calling `equals` on identical colors failed"
                , test "when colors are not identical, return false" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 100, 51 )
                            |> OpaqueColor.equals (OpaqueColor.fromRGB ( 255, 0, 0 ))
                            |> Expect.false "Calling `equals` on disparate colors failed"
                ]
            ]
        ]


conversionsSpec : Test
conversionsSpec =
    describe "Conversions & channel values"
        [ describe "toHSL"
            [ describe "from RGB color"
                [ test "black" <|
                    \_ ->
                        OpaqueColor.fromRGB ( 0, 0, 0 )
                            |> OpaqueColor.toHSL
                            |> expectTripleEquals ( 0, 0, 0 )
                , test "white" <|
                    \_ ->
                        OpaqueColor.fromRGB ( 255, 255, 255 )
                            |> OpaqueColor.toHSL
                            |> expectTripleEquals ( 0, 0, 100 )
                , test "red" <|
                    \_ ->
                        OpaqueColor.fromRGB ( 255, 0, 0 )
                            |> OpaqueColor.toHSL
                            |> expectTripleEquals ( 0, 100, 50 )
                , test "green" <|
                    \_ ->
                        OpaqueColor.fromRGB ( 0, 128, 0 )
                            |> OpaqueColor.toHSL
                            |> expectTripleEquals ( 120, 100, 25 )
                ]
            , describe "from HSL color"
                [ test "black" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 0, 0 )
                            |> OpaqueColor.toHSL
                            |> expectTripleEquals ( 0, 0, 0 )
                ]
            ]
        , describe "toRGB"
            [ describe "from RGB color"
                [ test "black" <|
                    \_ ->
                        OpaqueColor.fromRGB ( 0, 0, 0 )
                            |> OpaqueColor.toRGB
                            |> expectTripleEquals ( 0, 0, 0 )
                ]
            , describe "from HSL color"
                [ test "black" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 0, 0 )
                            |> OpaqueColor.toRGB
                            |> expectTripleEquals ( 0, 0, 0 )
                , test "white" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 0, 100 )
                            |> OpaqueColor.toRGB
                            |> expectTripleEquals ( 255, 255, 255 )
                , test "red" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 0, 100, 50 )
                            |> OpaqueColor.toRGB
                            |> expectTripleEquals ( 255, 0, 0 )
                , test "green" <|
                    \_ ->
                        OpaqueColor.fromHSL ( 120, 100, 25 )
                            |> OpaqueColor.toRGB
                            |> expectTripleEquals ( 0, 128, 0 )
                ]
            ]
        , fuzz OpaqueColorFuzzer.rgbValues "from RGB to HSL and back to RGB again" <|
            \color ->
                let
                    operations =
                        OpaqueColor.fromRGB
                            >> OpaqueColor.toHSL
                            >> OpaqueColor.fromHSL
                            >> OpaqueColor.toRGB

                    rgbName =
                        OpaqueColor.toRGBString (OpaqueColor.fromRGB color)
                in
                expectTripleEquals color (operations color)
        , fuzz OpaqueColorFuzzer.hslValues "from HSL to RGB and back to HSL again" <|
            \(( _, s, l ) as color) ->
                let
                    operations =
                        OpaqueColor.fromHSL
                            >> OpaqueColor.toRGB
                            >> OpaqueColor.fromRGB
                            >> OpaqueColor.toHSL

                    hslName =
                        OpaqueColor.toHSLString (OpaqueColor.fromHSL color)
                in
                if l == 0 then
                    -- This is black, which has more representations in HSL space
                    -- than in RGB space.
                    expectTripleEquals ( 0, 0, 0 ) (operations color)

                else if l == 100 then
                    -- This is white, which has more representations in HSL space
                    -- than in RGB space.
                    expectTripleEquals ( 0, 0, 100 ) (operations color)

                else if s == 0 then
                    -- This is a fully-desaturated gray. It also has more representations
                    -- in HSL space than in RGB space.
                    expectTripleEquals ( 0, 0, l ) (operations color)

                else
                    expectTripleEquals color (operations color)
        , fuzz (OpaqueColorFuzzer.hexStringOfLength 6) "from Hex to RGB and back to Hex again" <|
            \c ->
                OpaqueColor.fromHexString c
                    |> Result.map OpaqueColor.toRGB
                    |> Result.map (OpaqueColor.fromRGB >> OpaqueColor.toHexString)
                    |> Expect.equal (Ok c)
        ]


contrastSuite : Test
contrastSuite =
    describe "contrast"
        [ describe "black and white"
            [ test "contrast black white == contrast white black" <|
                \_ ->
                    Expect.equal (OpaqueColor.contrast black white) (OpaqueColor.contrast white black)
            , test "contrast black white" <|
                \_ ->
                    OpaqueColor.contrast black white
                        |> floatEqual 21
            , test "contrast white gray" <|
                \_ ->
                    OpaqueColor.contrast white gray
                        |> floatEqual (4.5 / 1)
            , test "contrast white white" <|
                \_ ->
                    OpaqueColor.contrast white white
                        |> floatEqual 1
            , test "contrast black black" <|
                \_ ->
                    OpaqueColor.contrast black black
                        |> floatEqual 1
            ]
        ]


sufficientContrastSuite : Test
sufficientContrastSuite =
    describe "sufficientContrast"
        [ describe "Regular sized text" <|
            let
                font =
                    { fontSize = 12, fontWeight = 300 }
            in
            [ describe "WCAG AA" <|
                let
                    subject =
                        OpaqueColor.sufficientContrast OpaqueColor.AA font
                in
                [ test "black and white has sufficient contrast" <|
                    \_ ->
                        subject white black
                            |> Expect.true "Expected black and white to have sufficient contrast."
                , test "gray and white do not have sufficient contrast" <|
                    \_ ->
                        subject white gray
                            |> Expect.true "Expected gray and white to have sufficient contrast."
                ]
            , describe "WCAG AAA" <|
                let
                    subject =
                        OpaqueColor.sufficientContrast OpaqueColor.AAA font
                in
                [ test "black and white has sufficient contrast" <|
                    \_ ->
                        subject white black
                            |> Expect.true "Expected black and white to have sufficient contrast."
                , test "gray and white do not have sufficient contrast" <|
                    \_ ->
                        subject white gray
                            |> Expect.false "Expected gray and white not to have sufficient contrast."
                ]
            ]
        , describe "Large text" <|
            let
                font =
                    { fontSize = 19, fontWeight = 300 }
            in
            [ describe "WCAG AA" <|
                let
                    subject =
                        OpaqueColor.sufficientContrast OpaqueColor.AA font
                in
                [ test "black and white has sufficient contrast" <|
                    \_ ->
                        subject white black
                            |> Expect.true "Expected black and white to have sufficient contrast."
                , test "gray and white has sufficient contrast" <|
                    \_ ->
                        subject white gray
                            |> Expect.true "Expected gray and white to have sufficient contrast."
                ]
            , describe "WCAG AAA" <|
                let
                    subject =
                        OpaqueColor.sufficientContrast OpaqueColor.AAA font
                in
                [ test "black and white has sufficient contrast" <|
                    \_ ->
                        subject white black
                            |> Expect.true "Expected black and white to have sufficient contrast."
                , test "gray and white to have sufficient contrast" <|
                    \_ ->
                        subject white gray
                            |> Expect.true "Expected gray and white to have sufficient contrast."
                ]
            ]
        ]


luminanceSuite : Test
luminanceSuite =
    describe "luminance"
        [ test "white is very bright" <|
            \_ ->
                white
                    |> OpaqueColor.luminance
                    |> floatEqual 1
        , test "gray is middlingly bright" <|
            \_ ->
                gray
                    |> OpaqueColor.luminance
                    |> floatEqual 0.215
        , test "black is not very bright" <|
            \_ ->
                black
                    |> OpaqueColor.luminance
                    |> floatEqual 0
        ]



--Test helpers


floatEqual : Float -> Float -> Expectation
floatEqual =
    Expect.within (Expect.Absolute 0.1)


gray : OpaqueColor
gray =
    OpaqueColor.fromRGB ( 118, 118, 118 )


{-| This exists mostly to make float equality checks nicer.
-}
expectRGB : ( Int, Int, Int ) -> OpaqueColor -> Expectation
expectRGB expected color =
    let
        ( r, g, b ) =
            OpaqueColor.toRGB color
    in
    Expect.equal ( round r, round g, round b ) expected


{-| This exists mostly to make float equality checks nicer.
-}
expectHSL : ( Int, Int, Int ) -> OpaqueColor -> Expectation
expectHSL expected color =
    let
        ( r, g, b ) =
            OpaqueColor.toHSL color
    in
    Expect.equal ( round r, round g, round b ) expected


expectHex : String -> Result String OpaqueColor -> Expectation
expectHex expected colorResult =
    case colorResult of
        Ok got ->
            Expect.equal expected (OpaqueColor.toHexString got)

        Err err ->
            Expect.fail ("Could not parse color string: \n" ++ err)


expectTripleEquals : ( Float, Float, Float ) -> ( Float, Float, Float ) -> Expectation
expectTripleEquals expected actual =
    Expect.equal (roundTriple actual) (roundTriple expected)


roundTriple : ( Float, Float, Float ) -> ( Int, Int, Int )
roundTriple ( a, b, c ) =
    ( round a, round b, round c )
