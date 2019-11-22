port module Main exposing (main)

import Browser
import Http
import Json.Decode as Decode exposing (Decoder, float, map, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Regex
import Types exposing (..)
import View


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = View.render
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port focusOn : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTyped value ->
            let
                newModel =
                    { model
                        | userInput = value
                        , positionDec = Nothing
                        , positionDms = Nothing
                        , positionW3w = Nothing
                        , parsedW3w = Nothing
                    }
                        |> convertInput
            in
            ( newModel, fetchRemoteCoords newModel )

        UserEnteredDegreesSymbol ->
            ( { model | userInput = model.userInput ++ "° " }, focusOn "input" )

        UserEnteredMinutesSymbol ->
            ( { model | userInput = model.userInput ++ "' " }, focusOn "input" )

        UserEnteredCommaSymbol ->
            ( { model | userInput = model.userInput ++ ", " }, focusOn "input" )

        GotW3w (Err _) ->
            ( { model | message = "w3w api error" }, Cmd.none )

        GotW3w (Ok words) ->
            ( { model | positionW3w = Just words }, Cmd.none )

        GotW3wCoords (Err _) ->
            ( { model | message = "w3w api error when fetching coords" }, Cmd.none )

        GotW3wCoords (Ok dec) ->
            ( { model
                | message = ""
                , positionDec = Just dec
                , positionDms = Just (dec2dms dec)
                , positionW3w = model.parsedW3w
              }
            , fetchRemoteCoords model
            )


init : String -> ( Model, Cmd Msg )
init flags =
    let
        initialModel =
            { userInput = ""
            , message = ""
            , inputIsValid = False
            , parsedW3w = Nothing
            , positionDec = Nothing
            , positionDms = Nothing
            , positionW3w = Nothing
            , w3wApiKey = flags
            }
    in
    ( initialModel, Cmd.none )


posDecRegex : Regex.Regex
posDecRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*(-?[0-9.]+)°?\\s*,\\s*(-?[0-9.]+)°?\\s*$"


posDmsRegex : Regex.Regex
posDmsRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([0-9]+)°\\s*([0-9]+)'\\s*([0-9.]+)\"?\\s*([EW])\\s*,\\s*([0-9]+)°\\s*([0-9]+)'\\s*([0-9.]+)\"?\\s*([NS])\\s*$"


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
    }



-- calculate all geolocation schemes from what3words


w3wDecoder : Decoder W3Words
w3wDecoder =
    Decode.succeed W3Words
        |> required "words" string


fetchRemoteCoords : Model -> Cmd Msg
fetchRemoteCoords model =
    case model.positionW3w of
        Nothing ->
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
                                { url = "https://api.what3words.com/v3/convert-to-3wa?coordinates=" ++ lonS ++ "%2C" ++ latS ++ "&key="++model.w3wApiKey
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
                        { url = "https://api.what3words.com/v3/convert-to-coordinates?key="++model.w3wApiKey++"&words=" ++ words ++ "&format=json"
                        , expect = Http.expectJson GotW3wCoords decoder
                        }

        Just _ ->
            Cmd.none
