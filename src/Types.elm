module Types exposing (..)

import Http
import Json.Encode


type Msg
    = UserTyped String
    | UserEnteredDegreesSymbol
    | UserEnteredMinutesSymbol
    | UserEnteredCommaSymbol
    | UserClickedClear
    | GotW3w (Result Http.Error (List String))
    | GotW3wCoords (Result Http.Error PositionDec)
    | UserClickedSetFindMe
    | UserClickedSetFindLocation
    | GotDeviceLocation PositionBrowser


type alias W3Words =
    { words : String }


type alias DecCoord =
    Float


type alias DmsCoord =
    { degrees : Float
    , minutes : Float
    , seconds : Float
    , direction : String
    }


type alias PositionDec =
    { lon : DecCoord, lat : DecCoord }


type alias PositionDms =
    { lon : DmsCoord, lat : DmsCoord }


type alias PositionW3w =
    List String


type alias PositionBrowser =
    { lon : Float, lat : Float, error : String }


type ViewType
    = FindLocation
    | FindMe


type RemoteData error value
    = NotAsked
    | Waiting
    | Failure error
    | Success value


type alias Model =
    { userInput : String
    , message : String
    , inputIsValid : Bool
    , parsedW3w : Maybe (List String) -- when user entered 3 words, but they've not been validated by API
    , positionDec : Maybe PositionDec
    , positionDms : Maybe PositionDms
    , positionW3w : RemoteData String PositionW3w
    , viewType : ViewType
    , browserLocation : RemoteData String PositionBrowser
    }


dec2dms : PositionDec -> PositionDms
dec2dms dec =
    let
        posLon =
            abs dec.lon

        lonDf =
            floor posLon |> toFloat

        lonM =
            (posLon - lonDf) * 60

        lonMf =
            floor lonM |> toFloat

        lonS =
            (lonM - lonMf) * 60

        lonSr =
            lonS |> roundTo precisionDms

        lonDir =
            if dec.lon > 0 then
                "E"

            else
                "W"

        posLat =
            abs dec.lat

        latDf =
            floor posLat |> toFloat

        latM =
            (posLat - latDf) * 60

        latMf =
            floor latM |> toFloat

        latS =
            (latM - latMf) * 60

        latSr =
            latS |> roundTo precisionDms

        latDir =
            if dec.lat > 0 then
                "N"

            else
                "S"
    in
    { lon = DmsCoord lonDf lonMf lonSr lonDir
    , lat = DmsCoord latDf latMf latSr latDir
    }



-- sets number of decimals to show for approximately 10m accuracy
-- https://en.wikipedia.org/wiki/Decimal_degrees#Precision


precisionDec =
    10000


precisionDms =
    100


dms2dec : PositionDms -> PositionDec
dms2dec dms =
    let
        lonAbs =
            dms.lon.degrees
                + dms.lon.minutes
                / 60
                + dms.lon.seconds
                / 3600
                |> roundTo precisionDec

        latAbs =
            dms.lat.degrees
                + dms.lat.minutes
                / 60
                + dms.lat.seconds
                / 3600
                |> roundTo precisionDec

        lon =
            if dms.lon.direction == "W" then
                -lonAbs

            else
                lonAbs

        lat =
            if dms.lat.direction == "S" then
                -latAbs

            else
                latAbs
    in
    PositionDec lon lat


roundTo : Int -> Float -> Float
roundTo n x =
    toFloat (round (x * toFloat n)) / toFloat n


fromHttpError : Http.Error -> String
fromHttpError error =
    case error of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus s ->
            "Bad status: " ++ String.fromInt s

        Http.BadBody s ->
            "Bad body: " ++ s
