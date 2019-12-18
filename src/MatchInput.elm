module MatchInput exposing (matchInput)

import Regex
import Types exposing (..)


matchInput : Model -> Model
matchInput model =
    matchInputTryOlc model


matchInputTryBng : Model -> Model
matchInputTryBng model =
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
                matchInputTryDec model


matchInputTryDec : Model -> Model
matchInputTryDec model =
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
            matchInputTryDms model


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


matchInputTryDms : Model -> Model
matchInputTryDms model =
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
            matchInputTryW3w model



-- Open Location Code


posOlcRegex : Regex.Regex
posOlcRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([a-zA-Z0-9]{8}\\+[a-zA-Z0-9]{2})\\s*$"


matchInputTryOlc : Model -> Model
matchInputTryOlc model =
    let
        matches : List Regex.Match
        matches =
            Regex.find posOlcRegex model.userInput
    in
    case matches of
        [ match ] ->
            case match.submatches of
                [ Just code ] ->
                    modelFromOlc True "Found Olc" (Just code) model

                _ ->
                    modelFromOlc False "Bad Olc match" Nothing model

        _ ->
            matchInputTryBng model



-- What3words


posW3wRegex : Regex.Regex
posW3wRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\s*([a-zA-Z]+)[^a-zA-Z]+([a-zA-Z]+)[^a-zA-Z]+([a-zA-Z]+)\\s*$"


matchInputTryW3w : Model -> Model
matchInputTryW3w model =
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


modelFromBng : Bool -> String -> Float -> Float -> Model -> Model
modelFromBng valid message easting northing model =
    { model
        | message = message
        , inputIsValid = valid
        , matchedGeocode =
            if valid then
                BNG

            else
                NoMatch
        , positionDec =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionW3w =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionOlc =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionBng =
            if valid then
                Success (PositionBng easting northing)

            else
                NeedToFetch
    }


modelFromOlc : Bool -> String -> Maybe String -> Model -> Model
modelFromOlc valid message olc model =
    { model
        | message = message
        , inputIsValid = valid
        , matchedGeocode =
            if valid then
                OLC

            else
                NoMatch
        , positionOlc =
            if valid then
                Success (olc |> Maybe.withDefault "")

            else
                NeedToFetch
        , positionDec =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionBng =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionW3w =
            if valid then
                NeedToFetch

            else
                NotAsked
    }


modelFromW3w : Bool -> String -> Maybe (List String) -> Model -> Model
modelFromW3w valid message words model =
    { model
        | message = message
        , parsedW3w = words
        , inputIsValid = valid
        , matchedGeocode =
            if valid then
                W3W

            else
                NoMatch
        , positionW3w =
            if valid then
                Success { words = words |> Maybe.withDefault [], nearestPlace = "" }

            else
                NeedToFetch
        , positionDec =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionBng =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionOlc =
            if valid then
                NeedToFetch

            else
                NotAsked
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
        , matchedGeocode =
            if valid then
                DMS

            else
                NoMatch
        , positionDec =
            if valid then
                Success (dms2dec dms)

            else
                NeedToFetch
        , positionW3w =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionBng =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionOlc =
            if valid then
                NeedToFetch

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
        , matchedGeocode =
            if valid then
                Dec

            else
                NoMatch
        , positionDec =
            if valid then
                Success dec

            else
                NotAsked
        , positionW3w =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionBng =
            if valid then
                NeedToFetch

            else
                NotAsked
        , positionOlc =
            if valid then
                NeedToFetch

            else
                NotAsked
    }
