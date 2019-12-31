module View exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import String exposing (fromFloat, fromInt, left)
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
    , Font.size 22
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
        [ text <| (String.left 100 message) ++ "... (v0.9.0)" ]


renderGeocodeGuess : Geocode -> Element Msg
renderGeocodeGuess code =
    paragraph positionBoxLabelStyle
        [ case code of
            Dec ->
                text "What you typed looks like lon/lat decimal"

            DMS ->
                text "What you typed looks like lon/lat degrees"

            W3W ->
                text "What you typed looks like what3words"

            BNG ->
                text "What you typed looks like eastings/northings"

            OLC ->
                text "What you typed looks like a Plus Code"

            NoMatch ->
                text ""
        ]


renderPage : Model -> List (Element Msg)
renderPage model =
    case model.viewType of
        FindMe ->
            [ row [ spacing 10, width fill ]
                [ renderBackButton
                , column [ width fill ]
                    [ renderTitle model
                    , renderAccuracy model.accuracy
                    ]
                , renderReload
                ]
            , renderPosBng model
            , renderPosDec model
--            , renderPosDms model
            , renderPosW3w model
            , renderPosOlc model
            , row [ centerX ] [ renderMapButton model.positionDec ]
            , renderDebugMessage model.message
            ]

        FindLocation ->
            [ row [ spacing 10 ] [ renderBackButton, renderTitle model ]
            , renderInputBox model
            , renderPosBng model
            , renderPosDec model
--            , renderPosDms model
            , renderPosW3w model
            , renderPosOlc model
            , row [ centerX ] [ renderMapButton model.positionDec ]
            , renderDebugMessage model.message
            ]

        SelectMode ->
            [ column
                [ padding 50, spacing 20, width fill, Font.center ]
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
    case model.positionDec of
        Failure error ->
            [ paragraph [ Font.color colour4 ] [ text <| "Error: " ++ error ] ]

        Success _ ->
            [ text "Your location " ]

        _ ->
            [ text "Finding your location" ]


renderReload : Element Msg
renderReload =
    Input.button
        (alignRight :: mapLinkStyle)
        { onPress = Just UserClickedRefresh
        , label = text "↻"
        }


renderAccuracy : Maybe Float -> Element Msg
renderAccuracy accuracy =
    let
        accStyle =
            [ Font.size 14, padding 3, Border.rounded 3 ]
    in
    case accuracy of
        Nothing ->
            el accStyle (text "Please wait")

        Just acc ->
            let
                t = 30
                m = 150

                accS =
                    acc |> round |> fromInt

                red =
                    (if acc < t then acc*m/t else m) |> round

                green =
                    (if acc < t then (t-acc)*m/t else 0) |> round

                blue =
                    0

                accColor =
                    Background.color (rgb255 red green blue)
            in
            el (accColor :: accStyle) (text ("accuracy: " ++ accS ++ "m"))


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
        , renderGeocodeGuess model.matchedGeocode
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


renderPosOlc : Model -> Element Msg
renderPosOlc model =
    let
        content =
            case model.positionOlc of
                Success pos ->
                    text pos
                Failure _ ->
                    text "Invalid Plus Code"
                _ ->
                    none
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "Plus Code")
        , content
        ]


renderPosDec : Model -> Element Msg
renderPosDec model =
    let
        content =
            case model.positionDec of
                Success pos ->
                    let
                        lonString =
                            pos.lon |> roundTo 100000 |> fromFloat

                        latString =
                            pos.lat |> roundTo 100000 |> fromFloat

                        dms =
                            dec2dms pos

                        lonDmsString =
                            fromFloat dms.lon.degrees
                                ++ "° "
                                ++ fromInt (round dms.lon.minutes)
                                ++ "′ "
                                ++ fromFloat (dms.lon.seconds |> roundTo 100)
                                ++ "″ "
                                ++ dms.lon.direction

                        latDmsString =
                            fromFloat dms.lat.degrees
                                ++ "° "
                                ++ fromInt (round dms.lat.minutes)
                                ++ "′ "
                                ++ fromFloat (dms.lat.seconds |> roundTo 100)
                                ++ "″ "
                                ++ dms.lat.direction
                    in
                    column
                        (width fill :: lonLatStyle)
                        [ row
                              [ width fill ]
                              [ "Longitude: " ++ lonString |> text
                              , el [ Font.size 16, alignRight ] ("(" ++ lonDmsString ++ ")" |> text) ]
                        , row
                              [ width fill ]
                              [ "Latitude: " ++ latString |> text
                              , el [ Font.size 16, alignRight ] ("(" ++ latDmsString ++ ")" |> text) ]
                        ]

                Failure _ ->
                    text "Error"

                _ ->
                    none
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "Lat/Lon")
        , content
        ]


renderPosW3w : Model -> Element Msg
renderPosW3w model =
    let
        w3wString =
            case model.positionW3w of
                NeedToFetch ->
                    text ""

                NotAsked ->
                    text ""

                WaitingForResponse ->
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


renderPosBng : Model -> Element Msg
renderPosBng model =
    let
        bngString =
            case model.positionEastingNorthing of
                NotAsked ->
                    text ""

                WaitingForResponse ->
                    text "(pending)"

                Success pos ->
                    column
                        lonLatStyle
                        [ "Easting: " ++ fromInt (round pos.easting) |> text
                        , "Northing: " ++ fromInt (round pos.northing) |> text
                        ]

                Failure message ->
                    text "(not found)"

                NeedToFetch ->
                    text ""
    in
    column
        positionBoxStyle
        [ el positionBoxLabelStyle (text "National Grid")
        , bngString
        ]


renderMapButton : RemoteData String PositionDec -> Element Msg
renderMapButton pos =
    case pos of
        Success { lat, lon } ->
            newTabLink
                mapLinkStyle
                { url = "https://www.google.com/maps/search/?api=1&query=" ++ fromFloat lat ++ "," ++ fromFloat lon
                , label = text "Open in Google Maps"
                }

        _ ->
            none


renderBackButton : Element Msg
renderBackButton =
    Input.button
        backButtonStyle
    <|
        { onPress = Just UserClickedBack
        , label = text "‹ back"
        }
