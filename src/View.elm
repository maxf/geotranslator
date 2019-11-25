module View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (fromFloat)
import Types exposing (..)


render : Model -> Html Msg
render model =
    div []
        [ h1 [ onClick UserClickedSetFindLocation ] [ text "find a location" ]
        , h1 [ onClick UserClickedSetFindMe ] [ text "find me" ]
        , renderInputBox model
        , div [] [ text model.message ]
        , renderPosDec model
        , renderPosDms model
        , renderPosW3w model
        , renderMapButton model.positionDec
        ]


renderMapButton : Maybe PositionDec -> Html Msg
renderMapButton pos =
    case pos of
        Nothing ->
            div [ class "map-button map-button-disabled" ]
                [ span
                    [ class "map-button-link" ]
                    [ "Open in Google Maps" |> text ]
                ]

        Just { lat, lon } ->
            div [ class "map-button" ]
                [ a
                    [ class "map-button-link"
                    , target "_blank"
                    , href ("https://www.google.com/maps/search/?api=1&query=" ++ fromFloat lat ++ "," ++ fromFloat lon)
                    ]
                    [ "Open in Google Maps" |> text ]
                ]


renderInputBox : Model -> Html Msg
renderInputBox model =
    case model.viewType of
        FindLocation ->
            renderFindLocationInput model

        FindMe ->
            renderFindMeInput model


renderFindMeInput : Model -> Html Msg
renderFindMeInput model =
    text ""


renderFindLocationInput : Model -> Html Msg
renderFindLocationInput model =
    let
        inputClass =
            if model.userInput == "" then
                ""

            else if model.inputIsValid then
                "validInput"

            else
                "invalidInput"
    in
    div [ class "input-area" ]
        [ input
            [ id "input"
            , onInput UserTyped
            , value model.userInput
            , class inputClass
            , placeholder "Type anything"
            ]
            []
        , button [ onClick UserEnteredDegreesSymbol ] [ text "°" ]
        , button [ onClick UserEnteredMinutesSymbol ] [ text "′" ]
        , button [ onClick UserEnteredCommaSymbol ] [ text "," ]
        , p [ class "example-input" ] [ text "for instance: 51.128172,-3.465142" ]
        , p [ class "example-input" ] [ text "or: 51° 7′ 41.4192″ N, 3° 27′ 54.5112″ W" ]
        , p [ class "example-input" ] [ text "or: incomes decide bronze" ]
        ]


renderPosDec : Model -> Html Msg
renderPosDec model =
    let
        content =
            case model.positionDec of
                Nothing ->
                    div [ class "result empty" ] [ text "" ]

                Just pos ->
                    div [ class "result" ]
                        [ fromFloat pos.lon ++ ", " ++ fromFloat pos.lat |> text ]
    in
    div []
        [ span [ class "result-header" ] [ text "lon/lat (decimal)" ]
        , content
        ]


renderPosW3w : Model -> Html Msg
renderPosW3w model =
    let
        w3wString =
            case model.positionW3w of
                Nothing ->
                    div [ class "result empty" ] [ text "" ]

                Just words ->
                    div [ class "result" ] [ String.join "." words |> text ]
    in
    div []
        [ span [ class "result-header" ] [ text "what3words" ]
        , w3wString
        ]


renderPosDms : Model -> Html Msg
renderPosDms model =
    let
        pos =
            case model.positionDms of
                Nothing ->
                    div [ class "result empty" ] [ text "" ]

                Just dms ->
                    div [ class "result" ]
                        [ fromFloat dms.lon.degrees
                            ++ "° "
                            ++ fromFloat dms.lon.minutes
                            ++ "′ "
                            ++ fromFloat dms.lon.seconds
                            ++ "\" "
                            ++ dms.lon.direction
                            ++ ", "
                            ++ fromFloat dms.lat.degrees
                            ++ "° "
                            ++ fromFloat dms.lat.minutes
                            ++ "′ "
                            ++ fromFloat dms.lat.seconds
                            ++ "\" "
                            ++ dms.lat.direction
                            |> text
                        ]
    in
    div []
        [ span [ class "result-header" ] [ text "lon/lat (DMS)" ]
        , pos
        ]
