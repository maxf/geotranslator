module View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (fromFloat)
import Types exposing (..)


render : Model -> Html Msg
render model =
    div []
        [ h1 [] [ text "find a location" ]
        , renderInputBox model
        , div [] [ text model.message ]
        , renderPosDec model
        , renderPosDms model
        ]


renderInputBox : Model -> Html Msg
renderInputBox model =
    let
        inputClass =
            if model.userInput == "" then
                ""

            else if model.inputIsValid then
                "validInput"

            else
                "invalidInput"
    in
    div []
        [ label [ for "input" ] [ text "Type anything " ]
        , input
            [ id "input"
            , onInput UserTyped
            , value model.userInput
            , class inputClass
            ]
            []
        , button [ onClick UserEnteredDegreesSymbol ] [ text "°" ]
        , button [ onClick UserEnteredMinutesSymbol ] [ text "'" ]
        , button [ onClick UserEnteredCommaSymbol ] [ text "," ]
        ]


renderPosDec : Model -> Html Msg
renderPosDec model =
    div []
        [ text
            ("lon/lat (decimal): "
                ++ fromFloat model.positionDec.lon
                ++ ", "
                ++ fromFloat model.positionDec.lat
            )
        ]


renderPosDms : Model -> Html Msg
renderPosDms model =
    div []
        [ text
            ("lon/lat (dms): "
                ++ fromFloat model.positionDms.lon.degrees
                ++ "° "
                ++ fromFloat model.positionDms.lon.minutes
                ++ "' "
                ++ fromFloat model.positionDms.lon.seconds
                ++ " "
                ++ model.positionDms.lon.direction
                ++ ", "
                ++ fromFloat model.positionDms.lat.degrees
                ++ "° "
                ++ fromFloat model.positionDms.lat.minutes
                ++ "' "
                ++ fromFloat model.positionDms.lat.seconds
                ++ " "
                ++ model.positionDms.lat.direction
            )
        ]
