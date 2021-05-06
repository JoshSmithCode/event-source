module View exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Event exposing (Event)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (EditTab(..), Model)
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
                [ div
                    [ class "btn btn-success"
                    , onClick SeedEvents
                    ]
                    [ text "Seed" ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-5" ]
                [ User.createForm model.createUserValue
                , Task.createForm model.createTaskValue
                ]
            , div
                [ class "col-7 list-group mt-4 event-log" ]
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
            [ class "row mt-2" ]
            [ div
                [ class "col-12" ]
                [ h4
                    []
                    [ text "Current Values" ]
                , table
                    [ class "table" ]
                    [ thead
                        []
                        (th
                            []
                            [ text "ID" ]
                            :: List.map (\heading -> th [] [ text heading ]) userHeaders
                        )
                    , tbody
                        []
                        (userRows
                            |> Dict.toList
                            |> List.sortBy Tuple.first
                            |> List.map (renderRow userHeaders)
                        )
                    ]
                ]
            ]
        ]





renderRow : List String -> ( Int, Dict String String ) -> Html Msg
renderRow headers ( id, data ) =
    tr
        []
        (td
            []
            [ text <| String.fromInt id ]
            :: List.map (renderCell data) headers
        )


renderCell : Dict String String -> String -> Html Msg
renderCell data col =
    td
        []
        [ Dict.get col data |> Maybe.withDefault "" |> text ]


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
