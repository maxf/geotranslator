port module Main exposing (main)

import Browser
import Regex
import Types exposing (..)
import View


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = View.render
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port focusOn : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTyped value ->
            ( { model | userInput = value } |> convertInput, Cmd.none )

        UserEnteredDegreesSymbol ->
            ( { model | userInput = model.userInput ++ "° " }, focusOn "input" )

        UserEnteredMinutesSymbol ->
            ( { model | userInput = model.userInput ++ "' " }, focusOn "input" )

        UserEnteredCommaSymbol ->
            ( { model | userInput = model.userInput ++ ", " }, focusOn "input" )


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { userInput = ""
            , message = ""
            , inputIsValid = False
            , positionDec =
                { lon = 0
                , lat = 0
                }
            , positionDms =
                { lon = DmsCoord 0 0 0 ""
                , lat = DmsCoord 0 0 0 ""
                }
            , positionW3w = []
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
                    modelFromW3w True "Found W3W" [ word1, word2, word3 ] model

                _ ->
                    modelFromW3w False "Bad W3W regexp matches" [ "", "", "" ] model

        _ ->
            modelFromW3w False "No W3W regexp matches" [ "", "", "" ] model


modelFromW3w : Bool -> String -> List String -> Model -> Model
modelFromW3w valid message words model =
    { model
        | message = message
        , inputIsValid = valid
        , positionW3w = words
    }
        |> convertFromW3w


modelFromDms : Bool -> String -> DmsCoord -> DmsCoord -> Model -> Model
modelFromDms valid message lon lat model =
    { model
        | message = message
        , inputIsValid = valid
        , positionDms = { lon = lon, lat = lat }
    }
        |> convertFromDms


modelFromDec : Bool -> String -> DecCoord -> DecCoord -> Model -> Model
modelFromDec valid message lon lat model =
    { model
        | message = message
        , inputIsValid = valid
        , positionDec = { lon = lon, lat = lat }
    }
        |> convertFromDec


roundTo : Int -> Float -> Float
roundTo n x =
    toFloat (round (x * toFloat n)) / toFloat n



-- calculate all geolocation schemes from lat/lon dms


convertFromDms : Model -> Model
convertFromDms model =
    let
        lonAbs =
            model.positionDms.lon.degrees
                + model.positionDms.lon.minutes
                / 60
                + model.positionDms.lon.seconds
                / 3600
                |> roundTo 100000

        latAbs =
            model.positionDms.lat.degrees
                + model.positionDms.lat.minutes
                / 60
                + model.positionDms.lat.seconds
                / 3600
                |> roundTo 100000

        lon =
            if model.positionDms.lon.direction == "W" then
                -lonAbs

            else
                lonAbs

        lat =
            if model.positionDms.lat.direction == "S" then
                -latAbs

            else
                latAbs
    in
    { model
        | positionDec = { lon = lon, lat = lat }
    }



-- calculate all geolocation schemes from lat/lon decimal


convertFromDec : Model -> Model
convertFromDec model =
    let
        posLon =
            abs model.positionDec.lon

        lonDf =
            floor posLon |> toFloat

        lonM =
            (posLon - lonDf) * 60

        lonMf =
            floor lonM |> toFloat

        lonS =
            (lonM - lonMf) * 60

        lonSr =
            toFloat (round (lonS * 1000)) / 1000

        lonDir =
            if model.positionDec.lon > 0 then
                "E"

            else
                "W"

        posLat =
            abs model.positionDec.lat

        latDf =
            floor posLat |> toFloat

        latM =
            (posLat - latDf) * 60

        latMf =
            floor latM |> toFloat

        latS =
            (latM - latMf) * 60

        latSr =
            toFloat (round (latS * 1000)) / 1000

        latDir =
            if model.positionDec.lat > 0 then
                "N"

            else
                "S"
    in
    { model
        | positionDms =
            { lon = DmsCoord lonDf lonMf lonSr lonDir
            , lat = DmsCoord latDf latMf latSr latDir
            }
    }

-- calculate all geolocation schemes from what3words


convertFromW3w : Model -> Model
convertFromW3w model =
    model
