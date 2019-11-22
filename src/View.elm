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
        [ input
            [ id "input"
            , onInput UserTyped
            , value model.userInput
            , class inputClass
            , placeholder "Type anything"
            ]
            []
        , button [ onClick UserEnteredDegreesSymbol ] [ text "°" ]
        , button [ onClick UserEnteredMinutesSymbol ] [ text "'" ]
        , button [ onClick UserEnteredCommaSymbol ] [ text "," ]
        , p [ class "example-input" ] [ text "for instance: 51.128172,-3.465142" ]
        , p [ class "example-input" ] [ text "or: 51° 7' 41.4192\" N, 3° 27' 54.5112\" W" ]
        , p [ class "example-input" ] [ text "or: incomes decide bronze" ]
        ]


renderPosDec : Model -> Html Msg
renderPosDec model =
    let
        posString =
            case model.positionDec of
                Nothing ->
                    []

                Just pos ->
                    [ fromFloat pos.lon, fromFloat pos.lat ]
    in
    div []
        [ span [ class "result-header" ] [ text "lon/lat (decimal)" ]
        , div [ class "result" ]
            [ String.join ", " posString |> text ]
        ]


renderPosW3w : Model -> Html Msg
renderPosW3w model =
    let
        w3wString =
            case model.positionW3w of
                Nothing ->
                    ""

                Just words ->
                    String.join "." words
    in
    div []
        [ span [ class "result-header" ] [ text "what3words" ]
        , div [ class "result" ] [ text w3wString ]
        ]


renderPosDms : Model -> Html Msg
renderPosDms model =
    let
        pos =
            case model.positionDms of
                Nothing ->
                    ""

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
        [ span [ class "result-header" ] [ text "lon/lat (DMS)" ]
        , div [ class "result" ] [ text pos ]
        ]
