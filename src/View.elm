module View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (fromFloat)
import Types exposing (..)


render : Model -> Html Msg
render model =
    div []
        [ renderTitle model
        , renderInputBox model
        , renderPosDec model
        , renderPosDms model
        , renderPosW3w model
        , renderMapButton model.positionDec
        , renderSwitchViewButton model.viewType
        , div [] [ text model.message ]
        ]


renderStatus : Model -> Html Msg
renderStatus model =
    case model.browserLocation of
        Waiting ->
            h2 []
                [ img [ id "spinner", src "spinner.gif", alt "please wait" ] []
                , text "Waiting for location"
                ]

        Failure error ->
            h2 [ class "error" ] [ "Error: " ++ error |> text ]

        Success _ ->
            h2 []
                [ img [ id "radar", src "radar.gif", alt "spinning radar" ] []
                , text "Tracking your location"
                ]

        _ ->
            text ""


renderTitle : Model -> Html Msg
renderTitle model =
    case model.viewType of
        FindMe ->
            div [] [ renderStatus model ]

        FindLocation ->
            h1 [] [ text "Find a location" ]


renderSwitchViewButton : ViewType -> Html Msg
renderSwitchViewButton viewType =
    case viewType of
        FindMe ->
            div
                [ class "switch-view-button"
                , onClick UserClickedSetFindLocation
                ]
                [ span
                    [ class "map-button-link" ]
                    [ text "Find another location" ]
                ]

        FindLocation ->
            div
                [ class "switch-view-button"
                , onClick UserClickedSetFindMe
                ]
                [ span
                    [ class "map-button-link" ]
                    [ text "Find me" ]
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
        , br [] []
        , button [ class "symbol", onClick UserEnteredDegreesSymbol ] [ text "°" ]
        , button [ class "symbol", onClick UserEnteredMinutesSymbol ] [ text "′" ]
        , button [ class "symbol", onClick UserEnteredCommaSymbol ] [ text "," ]
        , button [ class "clear", onClick UserClickedClear ] [ text "Clear" ]
        , p [ class "example-input" ] [ text "for instance: 51.128172,-3.465142" ]
        , p [ class "example-input" ] [ text "or: 51° 7′ 41.4192″ N, 3° 27′ 54.5112 W" ]
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
                    div [ class "result dec" ]
                        [ "Longitude: " ++ fromFloat pos.lon |> text
                        , br [] []
                        , "Latitude: " ++ fromFloat pos.lat |> text
                        ]
    in
    div []
        [ span [ class "result-header" ] [ text "Decimal" ]
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
                    div [ class "result w3w" ] [ String.join " " words |> text ]
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
                    div [ class "result dms" ]
                        [ "Longitude: " |> text
                        , br [] []
                        , fromFloat dms.lon.degrees
                            ++ " degrees, "
                            ++ fromFloat dms.lon.minutes
                            ++ " minutes, "
                            ++ fromFloat dms.lon.seconds
                            ++ " seconds "
                            ++ (if dms.lon.direction == "W" then
                                    "West"

                                else
                                    "East"
                               )
                            |> text
                        , br [] []
                        , "Latitude: " |> text
                        , br [] []
                        , fromFloat dms.lat.degrees
                            ++ " degrees, "
                            ++ fromFloat dms.lat.minutes
                            ++ " minutes, "
                            ++ fromFloat dms.lat.seconds
                            ++ " seconds "
                            ++ (if dms.lat.direction == "N" then
                                    "North"

                                else
                                    "South"
                               )
                            |> text
                        ]
    in
    div []
        [ span [ class "result-header" ] [ text "DMS" ]
        , pos
        ]
