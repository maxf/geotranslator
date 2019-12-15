port module Main exposing (main)

import Browser
import Http
import Json.Decode as Decode exposing (Decoder, float, map, map4, string)
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
        , positionDec = NotAsked
        , positionW3w = NotAsked
        , positionBng = NotAsked
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

        UserChoseFindLocation ->
            ( { model | viewType = FindLocation }, Cmd.none )

        UserChoseFindMe ->
            ( { model | viewType = FindMe }, getCurrentLocation "" )

        GotNewInputValue text ->
            ( model |> withNewUserInput text, Cmd.none )

        GotBngCoords (Err error) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | message = "BNG API error: " ++ fromHttpError error
                        , positionBng = Failure "Error"
                      }
                    , Cmd.none
                    )

        GotBngCoords (Ok apiResponse) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    let
                        posBng =
                            PositionBng apiResponse.easting apiResponse.northing

                        newModel =
                            { model
                                | message = ""
                                , positionBng = Success posBng
                            }
                    in
                    ( newModel, fetchRemoteCoords newModel )

        GotBngLatLon (Err error) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | message = "BNG API error: " ++ fromHttpError error
                        , positionBng = Failure "Error"
                      }
                    , Cmd.none
                    )

        GotBngLatLon (Ok apiResponse) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    let
                        dec =
                            PositionDec apiResponse.longitude apiResponse.latitude

                        bng =
                            PositionBng apiResponse.easting apiResponse.northing

                        newModel =
                            { model
                                | message = ""
                                , positionDec = Success dec
                                , positionBng = Success bng
                            }
                    in
                    ( newModel, fetchRemoteCoords newModel )

        GotW3wWords (Err error) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | message = "W3W API error: " ++ fromHttpError error
                        , positionW3w = Failure "Error"
                      }
                    , Cmd.none
                    )

        GotW3wWords (Ok w3wPosition) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    let
                        newModel =
                            { model
                                | message = ""
                                , positionW3w = Success (PositionW3w (String.split "." w3wPosition.words) w3wPosition.nearestPlace)
                            }
                    in
                    ( newModel, fetchRemoteCoords newModel )

        GotW3wCoords (Err error) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | message = "W3W API error: " ++ fromHttpError error
                        , positionW3w = Failure "got error from w3w"
                      }
                    , Cmd.none
                    )

        GotW3wCoords (Ok pos) ->
            case model.viewType of
                SelectMode ->
                    -- ignore callbacks if the user has gone back to start screen
                    ( model, Cmd.none )

                _ ->
                    let
                        dec =
                            PositionDec pos.coordinates.lng pos.coordinates.lat

                        w3w =
                            PositionW3w (String.split "." pos.words) pos.nearestPlace

                        newModel =
                            { model
                                | message = ""
                                , positionDec = Success dec
                                , positionW3w = Success w3w
                            }
                    in
                    ( newModel, fetchRemoteCoords newModel )

        UserClickedBack ->
            ( initialModel, stopGeolocation "" )

        GotDeviceLocation location ->
            case model.viewType of
                FindMe ->
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
                                    location.lon
                                    location.lat
                                    { model | browserLocation = Success location }
                        in
                        ( newModel, fetchRemoteCoords newModel )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


initialModel : Model
initialModel =
    { userInput = ""
    , message = ""
    , inputIsValid = False
    , parsedW3w = Nothing
    , positionDec = NotAsked
    , positionW3w = NotAsked
    , positionBng = NotAsked
    , viewType = SelectMode
    , browserLocation = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


posDecRegex : Regex.Regex
posDecRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*(-?[0-9.]+)°?\\s*,\\s*(-?[0-9.]+)°?\\s*$"


posDmsRegex : Regex.Regex
posDmsRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([0-9]+)°\\s*([0-9]+)[′']\\s*([0-9.]+)[\"″]?\\s*([NS])\\s*,\\s*([0-9]+)°\\s*([0-9]+)[′']\\s*([0-9.]+)[\"″]?\\s*([WE])\\s*$"


posBngRegex : Regex.Regex
posBngRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(-?[0-9]{6})[^-0-9A-Za-z](-?[0-9]{6})"


convertInput : Model -> Model
convertInput model =
    convertInputTryBng model


convertInputTryBng : Model -> Model
convertInputTryBng model =
    if model.userInput == "" then
        { model | message = "" }

    else
        let
            matches : List Regex.Match
            matches =
                Regex.find posBngRegex model.userInput
        in
        case matches of
            [ match ] ->
                case match.submatches of
                    [ Just eastingString, Just northingString ] ->
                        let
                            easting =
                                String.toFloat eastingString |> Maybe.withDefault 0

                            northing =
                                String.toFloat northingString |> Maybe.withDefault 0
                        in
                        modelFromBng True "Found BNG" easting northing model

                    _ ->
                        modelFromBng False "Bad BNG regex matches" 0 0 model

            _ ->
                convertInputTryDec model


convertInputTryDec : Model -> Model
convertInputTryDec model =
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
        , positionDec = Waiting
        , positionW3w =
            if valid then
                Success { words = words |> Maybe.withDefault [], nearestPlace = "" }

            else
                Waiting
        , positionBng = Waiting
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
        , positionDec =
            if valid then
                Success (dms2dec dms)

            else
                Waiting
        , positionW3w =
            if valid then
                Waiting

            else
                NotAsked
        , positionBng =
            if valid then
                Waiting

            else
                NotAsked
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
        , positionDec =
            if valid then
                Success dec

            else
                NotAsked
        , positionW3w =
            if valid then
                Waiting

            else
                NotAsked
        , positionBng =
            if valid then
                Waiting

            else
                NotAsked
    }


modelFromBng : Bool -> String -> Float -> Float -> Model -> Model
modelFromBng valid message easting northing model =
    let
        bng =
            PositionBng easting northing
    in
    { model
        | message = message
        , inputIsValid = valid
        , positionDec = Waiting
        , positionW3w = Waiting
        , positionBng =
            if valid then
                Success bng

            else
                Waiting
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


bngApiResponseDecoder : Decoder BngApiResponse
bngApiResponseDecoder =
    -- example: {"DEGMINSECLNG":{"DEGREES":-1,"SECONDS":24.028476096768,"MINUTES":32},"EASTING":429157,"LONGITUDE":-1.54000791002688,"NORTHING":623009,"DEGMINSECLAT":{"DEGREES":55,"SECONDS":59.99859710664,"MINUTES":29},"LATITUDE":55.4999996103074}
    -- example: {"DEGMINSECLNG":{"DEGREES":-1,"SECONDS":24,"MINUTES":32},"EASTING":451030.444044407,"LONGITUDE":-1.54,"ERROR":false,"DEGMINSECLAT":{"DEGREES":-5,"SECONDS":0,"MINUTES":33},"NORTHING":-6141064.83570885,"LATITUDE":-5.55}
    Decode.map4 BngApiResponse
        (Decode.field "LATITUDE" float)
        (Decode.field "LONGITUDE" float)
        (Decode.field "EASTING" float)
        (Decode.field "NORTHING" float)


fetchRemoteCoords : Model -> Cmd Msg
fetchRemoteCoords model =
    case model.positionDec of
        Success dec ->
            case model.positionBng of
                Success bng ->
                    case model.positionW3w of
                        Success _ ->
                            -- model is already complete
                            Cmd.none

                        _ ->
                            fetchW3wFromDec dec

                _ ->
                    fetchBngFromDec dec

        _ ->
            case model.positionBng of
                Success bng ->
                    fetchDecFromBng bng

                _ ->
                    case model.positionW3w of
                        Success w3w ->
                            fetchDecFromW3w w3w

                        _ ->
                            -- we have nothing to build model on
                            Cmd.none


fetchBngFromDec : PositionDec -> Cmd Msg
fetchBngFromDec pos =
    let
        lon =
            pos.lon |> String.fromFloat

        lat =
            pos.lat |> String.fromFloat
    in
    Http.get
        { url = "/api/bng/latlon2bng?lon=" ++ lon ++ "&lat=" ++ lat
        , expect = Http.expectJson GotBngCoords bngApiResponseDecoder
        }


fetchDecFromBng : PositionBng -> Cmd Msg
fetchDecFromBng pos =
    let
        east =
            pos.easting |> String.fromFloat

        north =
            pos.northing |> String.fromFloat
    in
    Http.get
        { url = "/api/bng/bng2latlon?easting=" ++ east ++ "&northing=" ++ north
        , expect = Http.expectJson GotBngLatLon bngApiResponseDecoder
        }


fetchW3wFromDec : PositionDec -> Cmd Msg
fetchW3wFromDec dec =
    let
        lonS =
            dec.lon |> String.fromFloat

        latS =
            dec.lat |> String.fromFloat
    in
    Http.get
        { url = "/api/w3w/c2w?lon=" ++ latS ++ "&lat=" ++ lonS
        , expect = Http.expectJson GotW3wWords w3wApiResponseDecoder
        }


fetchDecFromW3w : PositionW3w -> Cmd Msg
fetchDecFromW3w w3w =
    let
        words =
            String.join "." w3w.words
    in
    Http.get
        { url = "/api/w3w/w2c?words=" ++ words
        , expect = Http.expectJson GotW3wCoords w3wApiResponseDecoder
        }
