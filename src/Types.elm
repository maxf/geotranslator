module Types exposing (..)

import Http
import Json.Encode


type Msg
    = UserTyped String
    | UserEnteredSymbol String
    | UserClickedClear
    | GotW3wWords (Result Http.Error W3wApiResponse)
    | GotW3wCoords (Result Http.Error W3wApiResponse)
    | GotBngLatLon (Result Http.Error BngApiResponse)
    | GotBngCoords (Result Http.Error BngApiResponse)
    | UserClickedBack
    | GotDeviceLocation PositionBrowser
    | GotNewInputValue String
    | UserChoseFindLocation
    | UserChoseFindMe
    | NoOp


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
    { words : List String
    , nearestPlace : String
    }


type alias PositionBng =
    { easting : Float
    , northing : Float
    }


type alias PositionBrowser =
    { lon : Float, lat : Float, error : String }


type ViewType
    = SelectMode
    | FindLocation
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
    , viewType : ViewType
    , browserLocation : RemoteData String PositionBrowser

    -- when user entered 3 words, but they've not been validated by API:
    , parsedW3w : Maybe (List String)

    -- latitude/longitude in decimal form:
    , positionDec : RemoteData String PositionDec

    -- What3Words
    , positionW3w : RemoteData String PositionW3w

    -- British National Grid Eastings/Northings
    , positionBng : RemoteData String PositionBng
    }


type alias W3wApiResponseCoordinates =
    { lng : Float, lat : Float }


type alias W3wApiResponse =
    { words : String
    , nearestPlace : String
    , coordinates : W3wApiResponseCoordinates
    }


type alias BngApiResponse =
    { latitude : Float
    , longitude : Float
    , easting : Float
    , northing : Float
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

        latDir =
            if dec.lat > 0 then
                "N"

            else
                "S"
    in
    { lon = DmsCoord lonDf lonMf lonS lonDir
    , lat = DmsCoord latDf latMf latS latDir
    }



-- sets number of decimals to show for approximately 10m accuracy
-- https://en.wikipedia.org/wiki/Decimal_degrees#Precision


dms2dec : PositionDms -> PositionDec
dms2dec dms =
    let
        lonAbs =
            dms.lon.degrees
                + dms.lon.minutes
                / 60
                + dms.lon.seconds
                / 3600


        latAbs =
            dms.lat.degrees
                + dms.lat.minutes
                / 60
                + dms.lat.seconds
                / 3600

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
