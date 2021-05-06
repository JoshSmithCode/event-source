module Update exposing (..)

import Array
import Dict
import Event
import Model exposing (Model)
import Msg exposing (Msg(..))
import Set


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCreateUser string ->
            { model | createUserValue = string }

        SubmitCreateUser ->
            { model
                | events =
                    Array.push
                        (Event.CreateUser model.createUserValue)
                        model.events
                , createUserValue = ""
                , position = Array.length model.events + 1
            }

        UpdateCreateTask string ->
            { model | createTaskValue = string }

        SubmitCreateTask ->
            { model
                | events =
                    Array.push
                        (Event.CreateTask model.createTaskValue)
                        model.events
                , createTaskValue = ""
                , position = Array.length model.events + 1
            }

        Expand int ->
            { model | expanded = Set.insert int model.expanded }

        Collapse int ->
            { model | expanded = Set.remove int model.expanded }

        SetEditTab editTab ->
            { model | editTab = editTab }

        SelectEditUser int ->
            case Event.toCurrentUserRows model.position model.events |> Dict.get int of
                Nothing ->
                    model

                Just row ->
                    { model
                        | editingUser = Just int
                        , editingUserData =
                            Dict.toList row
                                |> Array.fromList
                    }

        SelectEditTask int ->
            case Event.toCurrentTaskRows model.position model.events |> Dict.get int of
                Nothing ->
                    model

                Just row ->
                    { model
                        | editingTask = Just int
                        , editingTaskDescription = row.description
                    }

        CancelEditingUser ->
            { model | editingUser = Nothing }

        CancelEditingTask ->
            { model | editingTask = Nothing }

        AddEditingRow ->
            { model | editingUserData = Array.push ("", "") model.editingUserData }

        UpdateEditingRowKey index newKey ->
            case Array.get index model.editingUserData of
                Nothing ->
                    model

                Just (_, value) ->
                    { model | editingUserData = Array.set index (newKey, value) model.editingUserData }


        UpdateEditingRowValue index newValue ->
            case Array.get index model.editingUserData of
                Nothing ->
                    model

                Just (key, _) ->
                    { model | editingUserData = Array.set index (key, newValue) model.editingUserData }

        SubmitEditUser ->
            case model.editingUser of
                Nothing ->
                    model

                Just id ->
                    let
                        newEvent = model.editingUserData
                            |> Array.toList
                            |> Dict.fromList
                            |> Event.UpdateUser id

                    in
                    { model
                        | events = Array.push newEvent model.events
                        , editingUser = Nothing
                        , editingUserData = Array.empty
                        , position = Array.length model.events + 1
                    }

        UpdateEditingTaskDescription description ->
            case model.editingTask of
                Nothing ->
                    model

                Just _ ->
                    { model | editingTaskDescription = description }

        SubmitEditingTaskDescription ->
            case model.editingTask of
                Nothing ->
                    model

                Just id ->
                    let
                        newEvent = Event.UpdateTaskDescription id model.editingTaskDescription
                    in
                    { model
                        | events = Array.push newEvent model.events
                        , editingTask = Nothing
                        , editingTaskDescription = ""
                        , position = Array.length model.events + 1
                    }

        SelectAssignUser userId ->
            case model.editingTask of
                Nothing ->
                    model

                Just _ ->
                    { model | editTaskAssignUser = Just userId }

        SubmitAssignUser ->
            case Maybe.map2 Event.AssignTask model.editingTask model.editTaskAssignUser of
                Just newEvent ->
                    { model
                        | events = Array.push newEvent model.events
                        , editingTask = Nothing
                        , editTaskAssignUser = Nothing
                        , position = Array.length model.events + 1
                    }

                Nothing ->
                    model


        UpdatePosition value ->
            case String.toInt value of
                Nothing ->
                    model

                Just position ->
                    { model | position = position }

        SeedEvents ->
            let
                events = Event.seedEvents
            in
            { model | events = events, position = Array.length events + 1 }

        SetTable table ->
            { model | table = table }
