module Palette.CubehelixSpec exposing (cubehelixRotationsSpec)

import Color exposing (Color)
import Expect exposing (Expectation)
import Fuzz
import Palette.Cubehelix as Cubehelix
import Palette.X11 exposing (black, white)
import Test exposing (..)


cubehelixRotationsSpec : Test
cubehelixRotationsSpec =
    describe "generate"
        [ describe "numLevels"
            [ test "negative numLevels" <|
                \() ->
                    Expect.fail "PENDING -- negative numLevels"
            , test "numLevels = 0" <|
                \() ->
                    assertGeneratesNumLevels 0
            , test "numLevels = 1" <|
                \() ->
                    assertGeneratesNumLevels 1
            , fuzz (Fuzz.intRange 0 256) "numLevels fuzz" <|
                \number ->
                    assertGeneratesNumLevels number
            , test "numLevels large numbers" <|
                \() ->
                    assertGeneratesNumLevels 1000
            ]
        , test "without hue" <|
            \() ->
                -- When the hue is zero, we should go straight to from black to white.
                case Cubehelix.generate { emptyConfig | numLevels = 2 } of
                    start :: end :: [] ->
                        expectColorsEqual white end

                    _ ->
                        Expect.fail "Uh oh -- `generate` didn't return the right number of levels."
        ]


assertGeneratesNumLevels : Int -> Expectation
assertGeneratesNumLevels numLevels =
    Cubehelix.generate { emptyConfig | numLevels = numLevels }
        |> List.length
        |> Expect.equal numLevels


emptyConfig : Cubehelix.AdvancedConfig
emptyConfig =
    { start = 0
    , rotations = 0
    , hue = 0
    , gamma = 0
    , numLevels = 0
    }


expectColorsEqual : Color -> Color -> Expectation
expectColorsEqual a b =
    Expect.equal (Color.toRGBString a) (Color.toRGBString b)
