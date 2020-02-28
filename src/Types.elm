module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict
import Http
import Json.Encode
import Regex
import String exposing (fromInt, slice)
import Url exposing (Url)


type Msg
    = GotDeviceLocation PositionBrowser
    | GotNewInputValue String
    | GotOlcFromDec ( Bool, PositionOlc )
    | GotDecFromOlc ( Bool, PositionDec )
    | GotOsgbFromDec ( Bool, PositionOsgb )
    | GotDecFromOsgb ( Bool, PositionDec )
    | GotW3wCoords (Result Http.Error W3wApiResponse)
    | GotW3wWords (Result Http.Error W3wApiResponse)
    | UserClickedLink UrlRequest
    | UserClickedClear
    | UserClickedRefresh
    | UserEnteredSymbol String
    | UserTyped String
    | UrlChanged Url
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
    { lon : DecCoord
    , lat : DecCoord
    }


type alias PositionDms =
    { lon : DmsCoord, lat : DmsCoord }


type alias PositionOlc =
    String


type alias PositionW3w =
    { words : List String
    , nearestPlace : String
    }


type alias PositionOsgb =
    { easting : Float
    , northing : Float
    }


type alias PositionBrowser =
    { lon : Float
    , lat : Float
    , accuracy : Float
    , error : String
    }


type ViewType
    = SelectMode
    | FindLocation
    | FindMe


type RemoteData error value
    = NotAsked -- no need to do anything
    | NeedToFetch -- will have to be fetch on next fetch loop
    | WaitingForResponse -- XHR request sent
    | Failure error -- XHR error
    | Success value -- success


type Geocode
    = Dec -- Longitude/latitude (decimal values)
    | DMS -- Longitude/latitude (degrees, minutes, seconds)
    | W3W -- What3Words
    | OSGB -- British National Grid
    | OLC -- Open Location Codes (aka Plus Codes)
    | NoMatch


type alias Model =
    { key : Browser.Navigation.Key
    , userInput : String
    , message : String
    , inputIsValid : Bool
    , viewType : ViewType
    , accuracy : Maybe Float -- meters

    -- what geocoding type has been recognised
    , matchedGeocode : Geocode

    -- when user entered 3 words, but they've not been validated by API:
    , parsedW3w : Maybe (List String)

    -- latitude/longitude in decimal form:
    , positionDec : RemoteData String PositionDec

    -- What3Words
    , positionW3w : RemoteData String PositionW3w

    -- OSGB (Eastings/Northings)
    , positionOsgb : RemoteData String PositionOsgb

    -- Open Location Codes
    , positionOlc : RemoteData String PositionOlc
    }


type alias W3wApiResponseCoordinates =
    { lng : Float, lat : Float }


type alias W3wApiResponse =
    { words : String
    , nearestPlace : String
    , coordinates : W3wApiResponseCoordinates
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


toNgrRef : PositionOsgb -> Maybe String
toNgrRef pos =
    let
        eastingAsString =
            pos.easting |> round |> String.fromInt

        northingAsString =
            pos.northing |> round |> String.fromInt

        osgbAsString =
            eastingAsString ++ northingAsString

        numLettersToKeep =
            if (String.length osgbAsString |> modBy 2) == 0 then
                2

            else
                3

        gridNumber =
            (eastingAsString |> String.slice 0 1)
                ++ (northingAsString |> String.slice 0 (numLettersToKeep - 1))

        gridLetters =
            Dict.get gridNumber gridRef
    in
    Maybe.map
        (\letters ->
            letters
                ++ " "
                ++ String.slice 1 5 eastingAsString
                ++ " "
                ++ String.slice 1 5 northingAsString
        )
        gridLetters


gridRef =
    Dict.fromList
        [ ( "012", "HL" )
        , ( "112", "HM" )
        , ( "212", "HN" )
        , ( "312", "HO" )
        , ( "412", "HP" )
        , ( "512", "JL" )
        , ( "612", "JM" )
        , ( "712", "JN" )
        , ( "011", "HQ" )
        , ( "111", "HR" )
        , ( "211", "HS" )
        , ( "311", "HT" )
        , ( "411", "HU" )
        , ( "511", "JQ" )
        , ( "611", "JR" )
        , ( "711", "JS" )
        , ( "010", "HV" )
        , ( "110", "HW" )
        , ( "210", "HX" )
        , ( "310", "HY" )
        , ( "410", "HZ" )
        , ( "510", "JV" )
        , ( "610", "JW" )
        , ( "710", "JX" )
        , ( "09", "NA" )
        , ( "19", "NB" )
        , ( "29", "NC" )
        , ( "39", "ND" )
        , ( "49", "NE" )
        , ( "59", "OA" )
        , ( "69", "OB" )
        , ( "79", "OC" )
        , ( "08", "NF" )
        , ( "18", "NG" )
        , ( "28", "NH" )
        , ( "38", "NJ" )
        , ( "48", "NK" )
        , ( "58", "OF" )
        , ( "68", "OG" )
        , ( "78", "OH" )
        , ( "07", "NL" )
        , ( "17", "NM" )
        , ( "27", "NN" )
        , ( "37", "NO" )
        , ( "47", "NP" )
        , ( "57", "OL" )
        , ( "67", "OM" )
        , ( "77", "ON" )
        , ( "06", "NQ" )
        , ( "16", "NR" )
        , ( "26", "NS" )
        , ( "36", "NT" )
        , ( "46", "NU" )
        , ( "56", "OQ" )
        , ( "66", "OR" )
        , ( "76", "OS" )
        , ( "05", "NV" )
        , ( "15", "NW" )
        , ( "25", "NX" )
        , ( "35", "NY" )
        , ( "45", "NZ" )
        , ( "55", "OV" )
        , ( "65", "OW" )
        , ( "75", "OX" )
        , ( "04", "SA" )
        , ( "14", "SB" )
        , ( "24", "SC" )
        , ( "34", "SD" )
        , ( "44", "SE" )
        , ( "54", "TA" )
        , ( "64", "TB" )
        , ( "74", "TC" )
        , ( "03", "SF" )
        , ( "13", "SG" )
        , ( "23", "SH" )
        , ( "33", "SJ" )
        , ( "43", "SK" )
        , ( "53", "TF" )
        , ( "63", "TG" )
        , ( "73", "TH" )
        , ( "02", "SL" )
        , ( "12", "SM" )
        , ( "22", "SN" )
        , ( "32", "SO" )
        , ( "42", "SP" )
        , ( "52", "TL" )
        , ( "62", "TM" )
        , ( "72", "TN" )
        , ( "01", "SQ" )
        , ( "11", "SR" )
        , ( "21", "SS" )
        , ( "31", "ST" )
        , ( "41", "SU" )
        , ( "51", "TQ" )
        , ( "61", "TR" )
        , ( "71", "TS" )
        , ( "00", "SV" )
        , ( "10", "SW" )
        , ( "20", "SX" )
        , ( "30", "SY" )
        , ( "40", "SZ" )
        , ( "50", "TV" )
        , ( "60", "TW" )
        , ( "70", "TX" )
        ]
