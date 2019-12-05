module View exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import String exposing (fromFloat)
import Types exposing (..)


colour1 =
    rgb255 0 0 42


colour2 =
    rgb255 0 119 151


colour3 =
    rgb255 66 104 185


colour4 =
    rgb255 238 130 28


colour5 =
    rgb255 150 200 150


positionBoxStyle =
    [ Background.color colour2
    , padding 8
    , spacing 5
    , width fill
    , Border.rounded 2
    ]


positionBoxLabelStyle =
    [ Font.size 12
    , Font.color colour4
    ]


mapLinkStyle =
    [ Background.color colour4
    , padding 10
    , Border.rounded 5
    ]




debugMessageStyle =
    [ Font.color (rgba 9 9 9 1)
    , Font.size 12
    ]


lonLatStyle =
    [ spacing 20 ]


symbolEntryStyle =
    [ Background.color colour3
    , padding 10
    , Border.rounded 5
    ]



render : Model -> Html Msg
render model =
    layout
        [ Background.color colour1
        , Font.color (rgba 1 1 1 1)
        , Font.size 19
        , Font.family [ Font.sansSerif ]
        ]
    <|
        column
            [ padding 5, spacing 10, width fill ]
            [ renderSwitchViewButton model.viewType
            , renderTitle model
            , renderInputBox model
            , renderPosDec model
            , renderPosDms model
            , renderPosW3w model
            , row [ centerX ] [ renderMapButton model.positionDec ]
            , el debugMessageStyle (text model.message)
            ]


renderTitle : Model -> Element Msg
renderTitle model =
    paragraph
        [ Region.heading 1
        , Font.size 32
        ]
    <|
        case model.viewType of
            FindMe ->
                renderStatus model

            FindLocation ->
                [ text "Find a location" ]


renderStatus : Model -> List (Element Msg)
renderStatus model =
    case model.browserLocation of
        Waiting ->
            [ text "Waiting for location" ]

        Failure error ->
            [ text <| "Error: " ++ error ]

        Success _ ->
            [ text "Your location:" ]

        _ ->
            [ none ]


renderInputBox : Model -> Element Msg
renderInputBox model =
    case model.viewType of
        FindLocation ->
            renderFindLocationInput model

        FindMe ->
            none


renderFindLocationInput : Model -> Element Msg
renderFindLocationInput model =
    let
        inputStyle =
            [ width fill
            , Font.color (rgba 0 0 0 1)
            , Background.color
                <| if model.inputIsValid then colour5 else rgb255 255 255 255
            ]
    in
    column
        [ spacing 15, width fill ]
        [ Input.search
            inputStyle
            { onChange = UserTyped
            , placeholder = Just (Input.placeholder [] (text "Type anything"))
            , text = model.userInput
            , label = Input.labelHidden "Type anything"
            }
        , wrappedRow
            [ spacing 10 ]
            [ Input.button symbolEntryStyle
                { onPress = Just UserEnteredDegreesSymbol
                , label = text "°"
                }
            , Input.button symbolEntryStyle
                { onPress = Just UserEnteredMinutesSymbol
                , label = text "′"
                }
            , Input.button symbolEntryStyle
                { onPress = Just UserEnteredCommaSymbol
                , label = text ","
                }
            , Input.button symbolEntryStyle
                { onPress = Just UserClickedClear
                , label = text "Clear"
                }
            ]
        , column
            [ Font.size 14 ]
            [ text "for instance: 51.128172,-3.465142"
            , text "or: 51° 7′ 41.4192″ N, 3° 27′ 54.5112 W"
            , text "or: incomes decide bronze"
            ]
        ]


renderPosDec : Model -> Element Msg
renderPosDec model =
    let
        content =
            case model.positionDec of
                Nothing ->
                    none

                Just pos ->
                    column
                        lonLatStyle
                        [ "Longitude: " ++ fromFloat pos.lon |> text
                        , "Latitude: " ++ fromFloat pos.lat |> text
                        ]
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "Decimal")
        , content
        ]


renderPosDms : Model -> Element Msg
renderPosDms model =
    let
        lonString dms =
            fromFloat dms.lon.degrees
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

        latString dms =
            fromFloat dms.lat.degrees
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

        pos =
            case model.positionDms of
                Nothing ->
                    none

                Just dms ->
                    column
                        lonLatStyle
                        [ paragraph [] [ "Longitude: " ++ lonString dms |> text ]
                        , paragraph [] [ "Latitude: " ++ latString dms |> text ]
                        ]
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "DMS")
        , pos
        ]


renderPosW3w : Model -> Element Msg
renderPosW3w model =
    let
        w3wString =
            case model.positionW3w of
                NotAsked ->
                    ""

                Waiting ->
                    "(pending)"

                Success words ->
                    String.join " " words

                Failure message ->
                    "(error)"

    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "what3words")
        , text w3wString
        ]


renderMapButton : Maybe PositionDec -> Element Msg
renderMapButton pos =
    case pos of
        Nothing ->
            none

        Just { lat, lon } ->
            newTabLink
                mapLinkStyle
                { url = "https://www.google.com/maps/search/?api=1&query=" ++ fromFloat lat ++ "," ++ fromFloat lon
                , label = text "Open in Google Maps"
                }


renderSwitchViewButton : ViewType -> Element Msg
renderSwitchViewButton viewType =
    Input.button
        [ Background.color colour3
        , padding 5
        , Border.rounded 5
        , Font.size 14
        , alignRight
        ]
    <|
        case viewType of
            FindMe ->
                { onPress = Just UserClickedSetFindLocation
                , label = text "Find another location"
                }

            FindLocation ->
                { onPress = Just UserClickedSetFindMe
                , label = text "Find me"
                }
