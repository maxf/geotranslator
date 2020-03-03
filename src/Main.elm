port module Main exposing (main)

import Browser
import Browser.Navigation
import Http
import Json.Decode as Decode exposing (Decoder, float, map, map4, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode exposing (Value)
import MatchInput exposing (matchInput)
import String exposing (fromFloat)
import Task
import Types exposing (..)
import Url exposing (Url)
import View


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = View.render
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UserClickedLink
        }



-- Ports


port getCurrentLocation : String -> Cmd msg


port stopGeolocation : String -> Cmd msg


port injectInputCharacter : ( String, String ) -> Cmd msg


port convertDecToOlc : ( Float, Float ) -> Cmd msg


port convertOlcToDec : String -> Cmd msg


port convertDecToOsgb : ( Float, Float ) -> Cmd msg


port convertOsgbToDec : ( Float, Float ) -> Cmd msg



-- Subscriptions


port gotDeviceLocation : (PositionBrowser -> msg) -> Sub msg


port injectedInputCharacter : (String -> msg) -> Sub msg


port convertedDecToOlc : (( Bool, PositionOlc ) -> msg) -> Sub msg


port convertedOlcToDec : (( Bool, PositionDec ) -> msg) -> Sub msg


port convertedDecToOsgb : (( Bool, PositionOsgb ) -> msg) -> Sub msg


port convertedOsgbToDec : (( Bool, PositionDec ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotDeviceLocation GotDeviceLocation
        , injectedInputCharacter GotNewInputValue
        , convertedDecToOlc GotOlcFromDec
        , convertedOlcToDec GotDecFromOlc
        , convertedDecToOsgb GotOsgbFromDec
        , convertedOsgbToDec GotDecFromOsgb
        ]


withNewUserInput : String -> Model -> Model
withNewUserInput value model =
    { model
        | userInput = value
        , positionDec = NotAsked
        , positionOsgb = NotAsked
        , positionOlc = NotAsked
    }
        |> matchInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedLink link ->
            case link of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            dispatchFromUrl (initialModel model.key) url.fragment

        UserTyped value ->
            fetchRemoteCoords (model |> withNewUserInput value)

        UserEnteredSymbol char ->
            ( model, injectInputCharacter ( char, "input" ) )

        UserClickedClear ->
            ( model |> withNewUserInput "", Cmd.none )

        UserClickedRefresh ->
            ( model, getCurrentLocation "" )

        GotNewInputValue text ->
            ( model |> withNewUserInput text, Cmd.none )

        GotDeviceLocation location ->
            case model.viewType of
                FindMe ->
                    if location.error /= "" then
                        ( { model
                            | positionDec = Failure location.error
                            , message = location.error
                          }
                        , Cmd.none
                        )

                    else
                        let
                            locationString =
                                fromFloat location.lat
                                    ++ ","
                                    ++ fromFloat location.lon

                            newModel =
                                model |> withNewUserInput locationString
                        in
                        fetchRemoteCoords { newModel | accuracy = Just location.accuracy }

                _ ->
                    ( model, Cmd.none )

        GotOlcFromDec ( error, olc ) ->
            fetchRemoteCoords
                { model
                    | positionOlc =
                        if error then
                            Failure "invalid"

                        else
                            Success (olc |> String.toUpper)
                }

        GotDecFromOlc ( error, dec ) ->
            fetchRemoteCoords
                { model
                    | positionDec =
                        if error then
                            NotAsked

                        else
                            Success dec
                    , positionOlc =
                        if error then
                            Failure "invalid"

                        else
                            model.positionOlc
                }

        GotOsgbFromDec ( error, osgb ) ->
            fetchRemoteCoords
                { model
                    | positionOsgb =
                        if error then
                            Failure "invalid"

                        else
                            Success osgb
                }

        GotDecFromOsgb ( error, dec ) ->
            fetchRemoteCoords
                { model
                    | positionDec =
                        if error then
                            NotAsked

                        else
                            Success dec
                    , positionOsgb =
                        if error then
                            Failure "invalid"

                        else
                            model.positionOsgb
                }

        NoOp ->
            ( model, Cmd.none )


initialModel : Browser.Navigation.Key -> Model
initialModel navKey =
    { key = navKey
    , userInput = ""
    , message = ""
    , inputIsValid = False
    , matchedGeocode = NoMatch
    , positionDec = NotAsked
    , positionOsgb = NotAsked
    , positionOlc = NotAsked
    , viewType = FindMe
    , accuracy = Nothing
    }


init : flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            initialModel navKey
    in
    dispatchFromUrl model url.fragment


fetchRemoteCoords : Model -> ( Model, Cmd Msg )
fetchRemoteCoords model =
    case model.positionDec of
        Success dec ->
            case model.positionOlc of
                NeedToFetch ->
                    case model.positionOsgb of
                        NeedToFetch ->
                            ( { model | positionOlc = WaitingForResponse, positionOsgb = WaitingForResponse }
                            , Cmd.batch [ fetchOlcFromDec dec, fetchOsgbFromDec dec ]
                            )

                        _ ->
                            ( { model | positionOlc = WaitingForResponse }
                            , fetchOlcFromDec dec
                            )

                _ ->
                    case model.positionOsgb of
                        NeedToFetch ->
                            ( { model | positionOsgb = WaitingForResponse }
                            , fetchOsgbFromDec dec
                            )

                        _ ->
                            ( model, Cmd.none )

        NeedToFetch ->
            case model.positionOlc of
                Success olc ->
                    ( { model | positionDec = WaitingForResponse }, fetchDecFromOlc olc )

                _ ->
                    case model.positionOsgb of
                        Success osgb ->
                            ( { model | positionDec = WaitingForResponse }, fetchDecFromOsgb osgb )

                        _ ->
                            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fetchDecFromOlc : PositionOlc -> Cmd Msg
fetchDecFromOlc olc =
    convertOlcToDec (olc |> String.toUpper)


fetchOlcFromDec : PositionDec -> Cmd Msg
fetchOlcFromDec dec =
    convertDecToOlc ( dec.lon, dec.lat )


fetchOsgbFromDec : PositionDec -> Cmd Msg
fetchOsgbFromDec dec =
    convertDecToOsgb ( dec.lon, dec.lat )


fetchDecFromOsgb : PositionOsgb -> Cmd Msg
fetchDecFromOsgb osgb =
    convertOsgbToDec ( osgb.easting, osgb.northing )


dispatchFromUrl : Model -> Maybe String -> ( Model, Cmd Msg )
dispatchFromUrl model maybeFrag =
    let
        frag =
            maybeFrag |> Maybe.withDefault ""
    in
    if frag == "start" then
        ( { model | viewType = SelectMode }, Cmd.none )

    else if frag == "locate" then
        ( { model | viewType = FindLocation }, stopGeolocation "" )

    else
        ( { model | viewType = FindMe }, getCurrentLocation "" )
