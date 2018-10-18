module Slider exposing (Config, view)

import Color exposing (Color, toRGBString)
import Color.Generator exposing (highContrast)
import Html exposing (Html)
import Html.Attributes exposing (attribute, id, style)
import Html.Events
import Html.Keyed
import Json.Decode


type alias Config msg =
    { setValue : Int -> msg
    , asColor : Int -> Color
    , valueMin : Int
    , valueMax : Int
    , valueNow : Int
    , labelId : String
    , labelText : String
    }


view : Config msg -> Html msg
view config =
    Html.div [ style "padding" "10px 36px 10px 10px" ]
        [ Html.Keyed.node "div"
            [ style "position" "relative" ]
            (slider config :: range config)
        , Html.label
            [ id config.labelId
            , style "display" "block"
            , style "text-align" "center"
            ]
            [ Html.text config.labelText ]
        ]


slider : Config msg -> ( String, Html msg )
slider { valueMin, valueMax, valueNow, setValue, labelId, labelText, asColor } =
    let
        border =
            style "border" ("1px solid " ++ toRGBString (highContrast (asColor valueNow)))

        increase =
            setValue (valueNow + 1)

        decrease =
            setValue (valueNow - 1)
    in
    ( labelText ++ "--" ++ "slider"
    , Html.div
        [ style "position" "relative"
        , style "margin" "auto"
        , style "top" (String.fromInt (valueMax - valueNow + 2) ++ "px")
        ]
        [ Html.div
            [ attribute "aria-role" "slider"
            , attribute "aria-valuemin" (String.fromInt valueMin)
            , attribute "aria-valuemax" (String.fromInt valueMax)
            , attribute "aria-valuenow" (String.fromInt valueNow)
            , attribute "aria-labelledby" labelId
            , attribute "aria-controls" (labelId ++ "-result")
            , attribute "tabindex" "0"
            , style "width" "100px"
            , style "height" "1px"
            , border
            , arrows { up = increase, down = decrease }
            ]
            []
        , Html.div
            [ attribute "aria-role" "presentation"
            , id (labelId ++ "-result")
            , style "position" "absolute"
            , style "width" "24px"
            , style "text-align" "center"
            , style "padding" "2px"
            , style "border-radius" "2px"
            , style "left" "100%"
            , style "top" "-10px"
            , style "background-color" "white"
            , border
            ]
            [ Html.text (String.fromInt valueNow) ]
        ]
    )


range : Config msg -> List ( String, Html msg )
range config =
    List.range config.valueMin config.valueMax
        |> List.reverse
        |> List.map (viewSlice config)


viewSlice : Config msg -> Int -> ( String, Html msg )
viewSlice config value =
    ( config.labelText ++ "--" ++ "slice" ++ "--" ++ String.fromInt value
    , Html.div
        [ style "width" "100px"
        , style "height" "1px"
        , style "margin" "auto"
        , style "background-color" (toRGBString (config.asColor value))
        , Html.Events.onClick (config.setValue value)
        ]
        []
    )


arrows : { up : msg, down : msg } -> Html.Attribute msg
arrows { up, down } =
    Html.Events.preventDefaultOn "keydown" <|
        Json.Decode.andThen
            (\keyCode ->
                if keyCode == 38 then
                    Json.Decode.succeed ( up, True )

                else if keyCode == 40 then
                    Json.Decode.succeed ( down, True )

                else
                    Json.Decode.fail (String.fromInt keyCode)
            )
            Html.Events.keyCode
