module Model exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Event exposing (Event)
import Set exposing (Set)


type alias Model =
    { events : Array Event
    , createUserValue : String
    , createTaskValue : String
    , createEventData : Dict String String
    , createEventUser : Maybe Int
    , position : Int
    , expanded : Set Int
    , editTab : EditTab
    , editingUser : Maybe Int
    , editingUserData : Array ( String, String )
    , editingTask : Maybe Int
    , editingTaskDescription : String
    , editTaskAssignUser : Maybe Int
    }


type EditTab
    = UserTab
    | TaskTab


initialModel : Model
initialModel =
    { events = Array.empty
    , createUserValue = ""
    , createTaskValue = ""
    , createEventData = Dict.empty
    , createEventUser = Nothing
    , position = 0
    , expanded = Set.empty
    , editTab = UserTab
    , editingUser = Nothing
    , editingUserData = Array.empty
    , editingTask = Nothing
    , editingTaskDescription = ""
    , editTaskAssignUser = Nothing
    }
