module Types exposing (..)


type Msg
    = UserTyped String
    | UserEnteredDegreesSymbol
    | UserEnteredMinutesSymbol
    | UserEnteredCommaSymbol


type alias DecCoord =
    Float


type alias DmsCoord =
    { degrees : Float
    , minutes : Float
    , seconds : Float
    , direction : String
    }


type alias Model =
    { userInput : String
    , message : String
    , inputIsValid : Bool
    , positionDec : { lat : DecCoord, lon : DecCoord }
    , positionDms :
        { lon : DmsCoord
        , lat : DmsCoord
        }
    , positionW3w : List String
    }
