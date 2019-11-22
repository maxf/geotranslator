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
        , renderPosW3w model
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
    let
        posString =
            case model.positionDec of
                Nothing ->
                    [ "n/a", "n/a" ]

                Just pos ->
                    [ fromFloat pos.lon, fromFloat pos.lat ]
    in
    div []
        [ "lon/lat (decimal): " ++ String.join ", " posString |> text ]


renderPosW3w : Model -> Html Msg
renderPosW3w model =
    let
        w3wString =
            case model.positionW3w of
                Nothing ->
                    "n/a"

                Just words ->
                    String.join "." words
    in
    div []
        [ "what3words: " ++ w3wString |> text ]


renderPosDms : Model -> Html Msg
renderPosDms model =
    let
        pos =
            case model.positionDms of
                Nothing ->
                    "n/a"

                Just dms ->
                    fromFloat dms.lon.degrees
                        ++ "° "
                        ++ fromFloat dms.lon.minutes
                        ++ "' "
                        ++ fromFloat dms.lon.seconds
                        ++ "\" "
                        ++ dms.lon.direction
                        ++ ", "
                        ++ fromFloat dms.lat.degrees
                        ++ "° "
                        ++ fromFloat dms.lat.minutes
                        ++ "' "
                        ++ fromFloat dms.lat.seconds
                        ++ "\" "
                        ++ dms.lat.direction
    in
    div []
        [ "lon/lat (dms): " ++ pos |> text ]
