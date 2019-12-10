module View exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
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


backButtonStyle =
    [ Background.color colour3
    , padding 8
    , Border.rounded 5
    , Font.size 14
    ]


positionBoxStyle =
    [ Background.color colour2
    , padding 8
    , spacing 5
    , width fill
    , Border.rounded 2
    , Font.size 24
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
            [ padding 5, spacing 15, width fill ]
            (renderPage model)


renderDebugMessage : String -> Element Msg
renderDebugMessage message =
    paragraph
        [ Font.color colour3
        , Font.size 12
        , alignRight
        ]
        [ text <| message ++ " (v0.04)" ]


renderPage : Model -> List (Element Msg)
renderPage model =
    case model.viewType of
        FindMe ->
            [ renderBackButton
            , renderTitle model
            , renderInputBox model
            , renderPosDec model
            , renderPosDms model
            , renderPosW3w model
            , row [ centerX ] [ renderMapButton model.positionDec ]
            , renderDebugMessage model.message
            ]

        FindLocation ->
            [ renderBackButton
            , renderTitle model
            , renderInputBox model
            , renderPosDec model
            , renderPosDms model
            , renderPosW3w model
            , row [ centerX ] [ renderMapButton model.positionDec ]
            , renderDebugMessage model.message
            ]

        SelectMode ->
            [ row
                [ padding 50, spacing 20, width fill ]
                [ Input.button startButtonStyle
                    { onPress = Just UserChoseFindMe
                    , label = text "Find me"
                    }
                , Input.button startButtonStyle
                    { onPress = Just UserChoseFindLocation
                    , label = text "Find a location"
                    }
                ]
            , renderDebugMessage model.message
            ]


startButtonStyle =
    [ Background.color colour3
    , padding 40
    , Font.center
    , centerX
    , width (px 200)
    , Border.rounded 10
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

            SelectMode ->
                []


renderStatus : Model -> List (Element Msg)
renderStatus model =
    case model.browserLocation of
        Waiting ->
            [ text "Finding your location" ]

        Failure error ->
            let
                secondMessage =
                    case model.positionDec of
                        Nothing ->
                            ". Retrying."

                        Just _ ->
                            ". Showing latest known location"
            in
            [ paragraph [ Font.color colour4 ] [ text <| "Error: " ++ error ++ secondMessage ] ]

        Success _ ->
            [ text "Your location:" ]

        _ ->
            [ none ]


renderInputBox : Model -> Element Msg
renderInputBox model =
    case model.viewType of
        FindLocation ->
            renderFindLocationInput model

        _ ->
            none


renderFindLocationInput : Model -> Element Msg
renderFindLocationInput model =
    let
        inputStyle =
            [ width fill
            , Font.color (rgba 0 0 0 1)
            , Background.color <|
                if model.inputIsValid then
                    colour5

                else
                    rgb255 255 255 255
            , htmlAttribute (Html.Attributes.id "input")
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
                { onPress = Just (UserEnteredSymbol "°")
                , label = text "°"
                }
            , Input.button symbolEntryStyle
                { onPress = Just (UserEnteredSymbol "′")
                , label = text "′"
                }
            , Input.button symbolEntryStyle
                { onPress = Just (UserEnteredSymbol ",")
                , label = text ","
                }
            , Input.button symbolEntryStyle
                { onPress = Just UserClickedClear
                , label = text "Clear"
                }
            ]
        , column
            [ Font.size 14 ]
            [ text "for instance: 51.12, -3.42"
            , text "or: 51° 7′ N, 3° 27′ 54.52 W"
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
                    text ""

                Waiting ->
                    text "(pending)"

                Success w3wPos ->
                    column
                        [ spacing 5 ]
                        [ text <| String.join " " w3wPos.words
                        , if w3wPos.nearestPlace == "" then
                            none

                          else
                            paragraph
                                [ Font.size 18, Font.color colour4 ]
                                [ text <| " (" ++ w3wPos.nearestPlace ++ ")" ]
                        ]

                Failure message ->
                    text "(not found)"
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "what3words")
        , w3wString
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


renderBackButton : Element Msg
renderBackButton =
    Input.button
        backButtonStyle
    <|
        { onPress = Just UserClickedBack
        , label = text "back"
        }
