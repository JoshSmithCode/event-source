module Model exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Event exposing (Event)
import Set exposing (Set)


type alias Model =
    { events : Array Event
    , createUser : String
    , createEventData : Dict String String
    , createEventUser : Maybe Int
    , position : Int
    , expanded : Set Int
    , editingUser : Maybe Int
    , editingUserData : Array ( String, String )
    }


initialModel : Model
initialModel =
    { events = Array.empty
    , createUser = ""
    , createEventData = Dict.empty
    , createEventUser = Nothing
    , position = 0
    , expanded = Set.empty
    , editingUser = Nothing
    , editingUserData = Array.empty
    }
