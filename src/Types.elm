module Types exposing (..)

import Http


type Msg
    = UserTyped String
    | UserEnteredDegreesSymbol
    | UserEnteredMinutesSymbol
    | UserEnteredCommaSymbol
    | GotW3w (Result Http.Error (List String))
    | GotW3wCoords (Result Http.Error PositionDec)


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

type alias Model =
    { userInput : String
    , message : String
    , inputIsValid : Bool
    , parsedW3w : Maybe (List String) -- when user entered 3 words, but they've not been validated by API
    , positionDec : Maybe PositionDec
    , positionDms : Maybe PositionDms
    , positionW3w : Maybe PositionW3w
    , w3wApiKey : String
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
            toFloat (round (lonS * 1000)) / 1000

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
            toFloat (round (latS * 1000)) / 1000

        latDir =
            if dec.lat > 0 then
                "N"

            else
                "S"
    in
    { lon = DmsCoord lonDf lonMf lonSr lonDir
    , lat = DmsCoord latDf latMf latSr latDir
    }


dms2dec : PositionDms -> PositionDec
dms2dec dms =
    let
        lonAbs =
            dms.lon.degrees
                + dms.lon.minutes
                / 60
                + dms.lon.seconds
                / 3600
                |> roundTo 100000

        latAbs =
            dms.lat.degrees
                + dms.lat.minutes
                / 60
                + dms.lat.seconds
                / 3600
                |> roundTo 100000

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
