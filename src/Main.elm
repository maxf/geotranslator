port module Main exposing (main)

import Browser
import Http
import Json.Decode as Decode exposing (Decoder, float, map, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode exposing (Value)
import Regex
import Task
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


port getCurrentLocation : String -> Cmd msg


port stopGeolocation : String -> Cmd msg


port injectInputCharacter : ( String, String ) -> Cmd msg


port gotDeviceLocation : (PositionBrowser -> msg) -> Sub msg


port injectedInputCharacter : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotDeviceLocation GotDeviceLocation
        , injectedInputCharacter GotNewInputValue
        ]


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

        UserEnteredSymbol char ->
            ( model, injectInputCharacter ( char, "input" ) )

        UserClickedClear ->
            ( model |> withNewUserInput "", Cmd.none )

        GotNewInputValue text ->
            ( model |> withNewUserInput text, Cmd.none )

        GotW3wWords (Err error) ->
            ( { model
                | message = "W3W API error: " ++ fromHttpError error
                , positionW3w = Failure "Error"
              }
            , Cmd.none
            )

        GotW3wWords (Ok w3wPosition) ->
            ( { model
                | message = ""
                , positionW3w = Success (PositionW3w (String.split "." w3wPosition.words) w3wPosition.nearestPlace)
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

        GotW3wCoords (Ok pos) ->
            let
                dec =
                    PositionDec pos.coordinates.lng pos.coordinates.lat

                w3w =
                    PositionW3w (String.split "." pos.words) pos.nearestPlace
            in
            ( { model
                | message = ""
                , positionDec = Just dec
                , positionDms = Just (dec2dms dec)
                , positionW3w = Success w3w
              }
            , Cmd.none
            )

        UserClickedSetFindLocation ->
            ( { initialModel
                | viewType = FindLocation
                , browserLocation = NotAsked }
            , stopGeolocation ""
            )

        UserClickedSetFindMe ->
            ( { initialModel | viewType = FindMe }, getCurrentLocation "" )

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

        NoOp ->
            ( model, Cmd.none )


initialModel : Model
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


init : ( Model, Cmd Msg )
init =
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
{-
   sample w3w responses when looking up words
   eg
   https://api.what3words.com/v3/convert-to-coordinates?words=filled.count.soap&key=[API-KEY]


   200 with:
   {"country":"GB","square":{"southwest":{"lng":-0.195543,"lat":51.520833},"northeast":{"lng":-0.195499,"lat":51.52086}},"nearestPlace":"Bayswater, London","coordinates":{"lng":-0.195521,"lat":51.520847},"words":"filled.count.soap"
   ,"language":"en","map":"https:\/\/w3w.co\/filled.count.soap"}

   400 with:
   {"error":{"code":"BadWords","message":"words must be a valid 3 word address, such as filled.count.soap or \/\/\/filled.count.soap"}}


   sample w3w responses when looking up coordinates:
   eg: https://api.what3words.com/v3/convert-to-3wa?coordinates=51.521251%2C-0.203586&key=[API-KEY]

   {"country":"ZZ","square":{"southwest":{"lng":0,"lat":0},"northeast":{"lng":0.000027,"lat":0.000027}},"nearestPlace":"","coordinates":{"lng":0.000013,"lat":0.000013},"words":"prosecuted.amplification.showings","language":"en","ma
   p":"https:\/\/w3w.co\/prosecuted.amplification.showings"}

-}


w3wApiResponseDecoder : Decoder W3wApiResponse
w3wApiResponseDecoder =
    let
        coordinatesDecoder =
            Decode.succeed W3wApiResponseCoordinates
                |> required "lng" float
                |> required "lat" float
    in
    Decode.succeed W3wApiResponse
        |> required "words" string
        |> required "nearestPlace" string
        |> required "coordinates" coordinatesDecoder


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
                    in
                    Http.get
                        { url = "/w3w/c2w?lon=" ++ latS ++ "&lat=" ++ lonS
                        , expect = Http.expectJson GotW3wWords w3wApiResponseDecoder
                        }

        Just parsedWords ->
            let
                words =
                    String.join "." parsedWords
            in
            Http.get
                { url = "/w3w/w2c?words=" ++ words
                , expect = Http.expectJson GotW3wCoords w3wApiResponseDecoder
                }
