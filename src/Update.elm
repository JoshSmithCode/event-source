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
            { model | createUser = string }

        SubmitCreateUser ->
            { model
                | events =
                    Array.push
                        (Event.CreateUser model.createUser)
                        model.events
                , createUser = ""
                , position = Array.length model.events + 1
            }

        Expand int ->
            { model | expanded = Set.insert int model.expanded }

        Collapse int ->
            { model | expanded = Set.remove int model.expanded }

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

        CancelEditing ->
            { model | editingUser = Nothing }

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

                Just index ->
                    let
                        newEvent = model.editingUserData
                            |> Array.toList
                            |> Dict.fromList
                            |> Event.UpdateUser index

                    in
                    { model
                        | events = Array.push newEvent model.events
                        , editingUser = Nothing
                        , editingUserData = Array.empty
                        , position = Array.length model.events + 1
                    }

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
