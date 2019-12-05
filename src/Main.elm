port module Main exposing (main)

import Browser
import Http
import Json.Decode as Decode exposing (Decoder, float, map, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode exposing (Value)
import Regex
import Types exposing (..)
import View


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = View.render
        , update = update
        , subscriptions = subscriptions
        }


port focusOn : String -> Cmd msg


port getCurrentLocation : String -> Cmd msg


port gotDeviceLocation : (PositionBrowser -> msg) -> Sub msg


port stopGeolocation : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    gotDeviceLocation GotDeviceLocation


withNewUserInput : String -> Model -> Model
withNewUserInput value model =
    { model
        | userInput = value
        , positionDec = Nothing
        , positionDms = Nothing
        , positionW3w = NotAsked
        , parsedW3w = Nothing
    }
        |> convertInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTyped value ->
            let
                newModel =
                    model |> withNewUserInput value
            in
            ( newModel, fetchRemoteCoords newModel )

        UserEnteredDegreesSymbol ->
            ( model |> withNewUserInput (model.userInput ++ "° "), focusOn "input" )

        UserEnteredMinutesSymbol ->
            ( model |> withNewUserInput (model.userInput ++ "′ "), focusOn "input" )

        UserEnteredCommaSymbol ->
            ( model |> withNewUserInput (model.userInput ++ ", "), focusOn "input" )

        UserClickedClear ->
            ( model |> withNewUserInput "", focusOn "input" )

        GotW3w (Err error) ->
            ( { model
                | message = "W3W API error: " ++ fromHttpError error
                , positionW3w = Failure "Error"
              }
            , Cmd.none
            )

        GotW3w (Ok words) ->
            ( { model
                | message = ""
                , positionW3w = Success words
              }
            , Cmd.none
            )

        GotW3wCoords (Err error) ->
            ( { model
                | message = "W3W API error: " ++ fromHttpError error
                , positionW3w = Failure "got error from w3w"
              }
            , Cmd.none
            )

        GotW3wCoords (Ok dec) ->
            ( { model
                | message = ""
                , positionDec = Just dec
                , positionDms = Just (dec2dms dec)
                , positionW3w =
                    case model.parsedW3w of
                        Nothing ->
                            NotAsked

                        Just words ->
                            Success words
              }
            , Cmd.none
            )

        UserClickedSetFindLocation ->
            ( { model
                | viewType = FindLocation
                , message = ""
                , inputIsValid = False
                , parsedW3w = Nothing
                , positionDec = Nothing
                , positionDms = Nothing
                , positionW3w = NotAsked
                , browserLocation = NotAsked
                , userInput = ""
              }
            , stopGeolocation ""
            )

        UserClickedSetFindMe ->
            ( { model
                | viewType = FindMe
                , browserLocation = Waiting
              }
            , getCurrentLocation ""
            )

        GotDeviceLocation location ->
            if location.error /= "" then
                ( { model
                    | browserLocation = Failure location.error
                    , message = location.error
                  }
                , Cmd.none
                )

            else
                let
                    newModel =
                        modelFromDec
                            True
                            "Got location from geolocation API"
                            (location.lon |> roundTo 100000)
                            (location.lat |> roundTo 100000)
                            { model | browserLocation = Success location }
                in
                ( newModel, fetchRemoteCoords newModel )


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { userInput = ""
            , message = ""
            , inputIsValid = False
            , parsedW3w = Nothing
            , positionDec = Nothing
            , positionDms = Nothing
            , positionW3w = NotAsked
            , viewType = FindMe
            , browserLocation = Waiting
            }
    in
    ( initialModel, getCurrentLocation "" )


posDecRegex : Regex.Regex
posDecRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*(-?[0-9.]+)°?\\s*,\\s*(-?[0-9.]+)°?\\s*$"


posDmsRegex : Regex.Regex
posDmsRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([0-9]+)°\\s*([0-9]+)[′']\\s*([0-9.]+)[\"″]?\\s*([NS])\\s*,\\s*([0-9]+)°\\s*([0-9]+)[′']\\s*([0-9.]+)[\"″]?\\s*([WE])\\s*$"


convertInput : Model -> Model
convertInput model =
    if model.userInput == "" then
        { model | message = "" }

    else
        let
            matches : List Regex.Match
            matches =
                Regex.find posDecRegex model.userInput
        in
        case matches of
            [ match ] ->
                case match.submatches of
                    [ Just lonString, Just latString ] ->
                        let
                            lon =
                                String.toFloat lonString |> Maybe.withDefault 0

                            lat =
                                String.toFloat latString |> Maybe.withDefault 0
                        in
                        if lon >= -180 && lon <= 180 && lat >= -90 && lat <= 90 then
                            modelFromDec True "Found decimal" lon lat model

                        else
                            modelFromDec False "Dec Outside limits" 0 0 model

                    _ ->
                        modelFromDec False "Bad Dec regex matches" 0 0 model

            _ ->
                convertInputTryDms model


dmsInBounds : DmsCoord -> Bool
dmsInBounds c =
    c.degrees
        >= 0
        && c.degrees
        <= 180
        && c.minutes
        >= 0
        && c.minutes
        < 60
        && c.seconds
        >= 0
        && c.seconds
        < 60
        && (c.direction == "E" || c.direction == "W" || c.direction == "N" || c.direction == "S")


convertInputTryDms : Model -> Model
convertInputTryDms model =
    let
        matches : List Regex.Match
        matches =
            Regex.find posDmsRegex model.userInput
    in
    case matches of
        [ match ] ->
            case match.submatches of
                [ Just lonDegString, Just lonMinString, Just lonSecString, Just lonDir, Just latDegString, Just latMinString, Just latSecString, Just latDir ] ->
                    let
                        lonDeg =
                            String.toFloat lonDegString |> Maybe.withDefault 0

                        lonMin =
                            String.toFloat lonMinString |> Maybe.withDefault 0

                        lonSec =
                            String.toFloat lonSecString |> Maybe.withDefault 0

                        latDeg =
                            String.toFloat latDegString |> Maybe.withDefault 0

                        latMin =
                            String.toFloat latMinString |> Maybe.withDefault 0

                        latSec =
                            String.toFloat latSecString |> Maybe.withDefault 0

                        lon =
                            DmsCoord lonDeg lonMin lonSec lonDir

                        lat =
                            DmsCoord latDeg latMin latSec latDir
                    in
                    if dmsInBounds lon && dmsInBounds lat then
                        modelFromDms True "Found DMS" lon lat model

                    else
                        modelFromDms False "DMS outside limits" (DmsCoord 0 0 0 "W") (DmsCoord 0 0 0 "N") model

                _ ->
                    modelFromDms False "Bad DMS regex matches" (DmsCoord 0 0 0 "W") (DmsCoord 0 0 0 "N") model

        _ ->
            convertInputTryW3w model


posW3wRegex : Regex.Regex
posW3wRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([a-zA-Z]+)[^a-zA-Z]+([a-zA-Z]+)[^a-zA-Z]+([a-zA-Z]+)\\s*$"


convertInputTryW3w : Model -> Model
convertInputTryW3w model =
    let
        matches : List Regex.Match
        matches =
            Regex.find posW3wRegex model.userInput
    in
    case matches of
        [ match ] ->
            case match.submatches of
                [ Just word1, Just word2, Just word3 ] ->
                    modelFromW3w True "Found W3W" (Just [ word1, word2, word3 ]) model

                _ ->
                    modelFromW3w False "Bad W3W regexp matches" Nothing model

        _ ->
            modelFromW3w False "No W3W regexp matches" Nothing model


modelFromW3w : Bool -> String -> Maybe (List String) -> Model -> Model
modelFromW3w valid message words model =
    { model
        | message = message
        , parsedW3w = words
        , inputIsValid = valid
    }


modelFromDms : Bool -> String -> DmsCoord -> DmsCoord -> Model -> Model
modelFromDms valid message lon lat model =
    let
        dms =
            PositionDms lon lat
    in
    { model
        | message = message
        , inputIsValid = valid
        , positionDms = Just dms
        , positionDec = Just (dms2dec dms)
        , positionW3w = Waiting
    }


modelFromDec : Bool -> String -> DecCoord -> DecCoord -> Model -> Model
modelFromDec valid message lon lat model =
    let
        dec =
            PositionDec lon lat
    in
    { model
        | message = message
        , inputIsValid = valid
        , positionDec = Just dec
        , positionDms = Just (dec2dms dec)
        , positionW3w = Waiting
    }



-- calculate all geolocation schemes from what3words


w3wDecoder : Decoder W3Words
w3wDecoder =
    Decode.succeed W3Words
        |> required "words" string


fetchRemoteCoords : Model -> Cmd Msg
fetchRemoteCoords model =
    case model.parsedW3w of
        Nothing ->
            case model.positionDec of
                Nothing ->
                    Cmd.none

                Just pos ->
                    let
                        lonS =
                            pos.lon |> String.fromFloat

                        latS =
                            pos.lat |> String.fromFloat

                        decoder =
                            Decode.map
                                (\s -> String.split "." s.words)
                                w3wDecoder
                    in
                    Http.get
                        { url = "/w3w/c2w?lon=" ++ lonS ++ "&lat=" ++ latS
                        , expect = Http.expectJson GotW3w decoder
                        }

        Just parsedWords ->
            let
                words =
                    String.join "." parsedWords

                decoder =
                    Decode.succeed PositionDec
                        |> requiredAt [ "coordinates", "lng" ] float
                        |> requiredAt [ "coordinates", "lat" ] float
            in
            Http.get
                { url = "/w3w/w2c?words=" ++ words
                , expect = Http.expectJson GotW3wCoords decoder
                }
