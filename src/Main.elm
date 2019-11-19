module Main exposing (main)

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTyped value ->
            ( { model | userInput = value } |> convertInput, Cmd.none )

        UserEnteredDegreesSymbol ->
            ( { model | userInput = model.userInput ++ "° " }, Cmd.none )

        UserEnteredMinutesSymbol ->
            ( { model | userInput = model.userInput ++ "' " }, Cmd.none )

        UserEnteredCommaSymbol ->
            ( { model | userInput = model.userInput ++ ", " }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { userInput = ""
            , message = ""
            , inputIsValid = False
            , positionDec = { lon = 0, lat = 0 }
            , positionDms =
                { lon = { degrees = 0, minutes = 0, seconds = 0, direction = "" }
                , lat = { degrees = 0, minutes = 0, seconds = 0, direction = "" }
                }
            }
    in
    ( initialModel, Cmd.none )


posDecRegex : Regex.Regex
posDecRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)"


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
                [match] ->
                    case match.submatches of
                        [ Just lonString, Just latString ] ->
                            let
                                lon = String.toFloat lonString |> Maybe.withDefault 0
                                lat = String.toFloat latString |> Maybe.withDefault 0
                            in
                                if lon >= -180 && lon <= 180 && lat >= -90 && lat <= 90 then
                                    modelFromDec True "OK" lon lat model
                                else
                                    modelFromDec False "Outside limits" 0 0 model
                        _ ->
                            modelFromDec False "Bad regex matches" 0 0 model
                _ ->
                    modelFromDec False "Input not recognised" 0 0 model


modelFromDec : Bool -> String -> Float -> Float -> Model -> Model
modelFromDec valid message lon lat model =
    { model
        | message = message
        , inputIsValid = valid
        , positionDec = { lon = lon, lat = lat }
    } |> convertFromDec

-- calculate all geolocation schemes from lat/lon deciman
convertFromDec : Model -> Model
convertFromDec model =
    let
        posLon = abs model.positionDec.lon
        lonDf = floor posLon |> toFloat
        lonM = (posLon - lonDf) * 60
        lonMf = floor lonM |> toFloat
        lonS = (lonM - lonMf) * 60
        lonSr = toFloat (round (lonS * 1000)) / 1000
        lonDir = if model.positionDec.lon > 0 then "E" else "W"

        posLat = abs model.positionDec.lat
        latDf = floor posLat |> toFloat
        latM = (posLat - latDf) * 60
        latMf = floor latM |> toFloat
        latS = (latM - latMf) * 60
        latSr = toFloat (round (latS * 1000)) / 1000
        latDir = if model.positionDec.lat > 0 then "N" else "S"
    in
        { model
              | positionDms =
                  { lon = { degrees = lonDf, minutes = lonMf, seconds = lonSr, direction = lonDir }
                  , lat = { degrees = latDf, minutes = latMf, seconds = latSr, direction = latDir }
                  }
        }
