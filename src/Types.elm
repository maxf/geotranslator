module Types exposing (..)


type Msg
    = UserTyped String
    | UserEnteredDegreesSymbol
    | UserEnteredMinutesSymbol
    | UserEnteredCommaSymbol


type alias Model =
    { userInput : String
    , message : String
    , positionDec : { lon : Float, lat : Float }
    , positionDms :
        { lon : { degrees : Float, minutes : Float, seconds : Float }
        , lat : { degrees : Float, minutes : Float, seconds : Float }
        }
    }
