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
            , positionDec = { lon = 0, lat = 0 }
            , positionDms =
                { lon = { degrees = 0, minutes = 0, seconds = 0 }
                , lat = { degrees = 0, minutes = 0, seconds = 0 }
                }
            }
    in
    ( initialModel, Cmd.none )


convertInput : Model -> Model
convertInput model =
    if model.userInput == "" then
        { model | message = "" }
    else
        let
            posDecRegex : Regex.Regex
            posDecRegex =
                Maybe.withDefault Regex.never <|
                    Regex.fromString "([-0-9.])\\s+,\\s+([-0-9.]))"

            matches : List Regex.Match
            matches =
                Regex.find posDecRegex model.userInput
        in
            if List.length matches == 2 then
                { model | message = "OK" }
            else
                { model | message = "Input not recognised yet" }
