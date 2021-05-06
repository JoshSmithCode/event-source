module View exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Event exposing (Event, ProductivityRow)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (EditTab(..), Model, Table(..))
import Msg exposing (Msg(..))
import Set exposing (Set)
import View.Task as Task
import View.User as User


view : Model -> Html Msg
view model =
    let
        userHeaders =
            Event.toCurrentUserHeaders model.position model.events
                |> Set.toList

        userRows =
            Event.toCurrentUserRows model.position model.events

        taskRows =
            Event.toCurrentTaskRows model.position model.events
    in
    div
        [ class "container" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-12" ]
                [ h3
                    []
                    [ text "Event Sourcing Demo" ]
                , p
                    []
                    [ text "This demo exists as a simple example of the possibilities of event sourcing. Using the forms below you can create events, then in the tables underneath you can see how we're able to create different 'tables' or 'visualisations' of your events. There's also a handy time-traveller that will allow you to move forward and backwards through events to watch how your data has changed over time." ]
                ]
            ]
        , hr
            []
            []
        , div
            [ class "row" ]
            [ div
                [ class "col-10 d-flex align-items-center" ]
                [ p
                    [ class "mb-0" ]
                    [ text "Don't have a lot of time to spend creating your own events? Just hit this seed button to generate some data to play with." ]
                ]
            , div
                [ class "col-2" ]
                [ div
                    [ class "btn btn-success"
                    , onClick SeedEvents
                    ]
                    [ text "Seed" ]
                ]
            ]
        , hr
            []
            []
        , div
            [ class "row" ]
            [ div
                [ class "col-md-5" ]
                [ User.createForm model.createUserValue
                , Task.createForm model.createTaskValue
                ]
            , div
                [ class "col-md-7 list-group mt-4 event-log" ]
                (div
                    [ class "list-group-item list-group-item-primary sticky-top"
                    ]
                    [ text "Event Log" ]
                    :: (Array.indexedMap (showEvent model.expanded) model.events |> Array.toList)
                )
            ]
        , hr
            []
            []
        , div
            [ class "row mt-2" ]
            [ div
                [ class "col-12" ]
                [ h4
                    []
                    [ text "Time Travelling" ]
                ]
            , div
                [ class "col-12" ]
                [ input
                    [ type_ "range"
                    , step "1"
                    , Attributes.min "0"
                    , Attributes.max <| String.fromInt <| Array.length model.events
                    , value <| String.fromInt model.position
                    , class "form-control"
                    , onInput UpdatePosition
                    ]
                    []
                ]
            ]
        , hr
            []
            []
        , div
            [ class "nav nav-tabs" ]
            [ div
                [ class "nav-item" ]
                [ a
                    [ class "nav-link pointer"
                    , classList [("active", model.editTab == UserTab)]
                    , onClick <| SetEditTab UserTab
                    ]
                    [ text "User Edit" ]
                ]
            , div
                [ class "nav-item" ]
                [ a
                    [ class "nav-link pointer"
                    , classList [("active", model.editTab == TaskTab)]
                    , onClick <| SetEditTab TaskTab
                    ]
                    [ text "Task Edit" ]
                ]
            ]
        , (
            case model.editTab of
                UserTab ->
                    User.editForm userRows model.editingUser model.editingUserData

                TaskTab ->
                    Task.editForm taskRows userRows model.editingTask model.editingTaskDescription
        )
        , hr
            []
            []
        , div
            [ class "nav nav-tabs" ]
            [ div
                [ class "nav-item" ]
                [ a
                    [ class "nav-link pointer"
                    , classList [("active", model.table == UserTable)]
                    , onClick <| SetTable UserTable
                    ]
                    [ text "User Table" ]
                ]
            , div
                [ class "nav-item" ]
                [ a
                    [ class "nav-link pointer"
                    , classList [("active", model.table == TaskTable)]
                    , onClick <| SetTable TaskTable
                    ]
                    [ text "Task Table" ]
                ]
            , div
                [ class "nav-item" ]
                [ a
                    [ class "nav-link pointer"
                    , classList [("active", model.table == ProductivityTable)]
                    , onClick <| SetTable ProductivityTable
                    ]
                    [ text "Productivity Table" ]
                ]
            ]
        , (
            case model.table of
                UserTable ->
                    User.tableView userHeaders userRows

                TaskTable ->
                    Task.tableView taskRows

                ProductivityTable ->
                    productivityTableView model.position model.events
        )
        ]


productivityTableView : Int -> Array Event -> Html Msg
productivityTableView position events =
    let
        rows = Event.toCurrentProductivityRows position events
    in
    div
        [ class "row" ]
        [ div
            [ class "col-12" ]
            [ table
                [ class "table table-bordered table-sm border-top-0" ]
                [ thead
                    []
                    [ th
                        [ class "border-top-0"]
                        [ text "ID" ]
                    , th
                        [ class "border-top-0"]
                        [ text "Name" ]
                    , th
                        [ class "border-top-0"]
                        [ text "Active Tasks" ]
                    , th
                        [ class "border-top-0"]
                        [ text "Completed Tasks" ]
                    ]
                , tbody
                    []
                    (rows
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map renderRow
                    )
                ]
            ]
        ]


renderRow : (Int, ProductivityRow) -> Html Msg
renderRow ( id, { name, activeTasks, completedTasks }) =
    tr
        []
        [ td
            []
            [ text <| String.fromInt id ]
        , td
            []
            [ text name ]
        , td
            []
            [ Set.size activeTasks |> String.fromInt |> text ]
        , td
            []
            [ Set.size completedTasks |> String.fromInt |> text ]
        ]


showEvent : Set Int -> Int -> Event -> Html Msg
showEvent expanded index event =
    let
        isExpanded =
            Set.member index expanded
    in
    div
        [ class "list-group-item p-2" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col" ]
                [ p
                    [ class "bold mb-0" ]
                    [ text <| Event.toName event ]
                ]
            , div
                [ class "col d-flex justify-content-end" ]
                [ div
                    [ class "btn btn-light btn-sm"
                    , onClick <|
                        if isExpanded then
                            Collapse index

                        else
                            Expand index
                    ]
                    [ text <|
                        if isExpanded then
                            "collapse"

                        else
                            "expand"
                    ]
                ]
            ]
        , if isExpanded then
            Event.renderBody event

          else
            text ""
        ]
