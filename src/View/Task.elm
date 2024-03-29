module View.Task exposing (..)

import Dict exposing (Dict)
import Event exposing (TaskRow)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msg exposing (Msg(..))


createForm : String -> Html Msg
createForm createTaskValue =
     div
         [ class "form-group" ]
         [ label
             []
             [ text "Create Task" ]
         , input
             [ type_ "text"
             , value createTaskValue
             , onInput UpdateCreateTask
             , class "form-control"
             ]
             []
         , div
             [ class "btn btn-primary mt-1"
             , onClick SubmitCreateTask
             ]
             [ text "Add Task" ]
         ]


editForm : Dict Int TaskRow -> Dict Int (Dict String String) -> Maybe Int -> String -> Html Msg
editForm taskRows userRows maybeTaskId editingTaskDescription =
    div
        [ class "row mt-2" ]
        [ div
            [ class "col-12" ]
            [ h4
                []
                [ text "Edit Task" ]
            ]
        , maybeTaskId
            |> Maybe.andThen (\id -> Dict.get id taskRows)
            |> Maybe.map (renderEdit userRows editingTaskDescription)
            |> Maybe.withDefault (editTaskSelect taskRows)
        ]


renderEdit : Dict Int (Dict String String) -> String -> TaskRow -> Html Msg
renderEdit userRows descriptionValue { name, description, assignedUserId, isComplete } =
    div
        [ class "col-12" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-9" ]
                [ span
                    []
                    [ text <| name ++ " - " ]
                , span
                    []
                    [ text description ]
                ]
            , div
                [ class "col-3 d-flex justify-content-end" ]
                [ div
                    [ class "btn btn-danger"
                    , onClick CancelEditingTask
                    ]
                    [ text "Cancel" ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-lg-6 col-12" ]
                [ descriptionForm descriptionValue ]
            , div
                [ class "col-lg-6 col-12" ]
                [ assignForm assignedUserId userRows ]
            ]
        ]


descriptionForm : String -> Html Msg
descriptionForm descriptionValue =
    div
        [ class "form-group" ]
        [ label
            []
            [ strong [] [ text "Task Description" ] ]
        , input
            [ type_ "text"
            , value descriptionValue
            , onInput UpdateEditingTaskDescription
            , class "form-control"
            ]
            []
        , div
            [ class "btn btn-success mt-2"
            , onClick SubmitEditingTaskDescription
            ]
            [ text "Submit Description" ]
        ]


assignForm : Maybe Int -> Dict Int (Dict String String) -> Html Msg
assignForm assignedUserId userRows =
    case assignedUserId of
        Nothing ->
            div
                [ class "form-group" ]
                [ label
                    []
                    [ strong [] [text "Assign User" ] ]
                , select
                    [ class "form-control" ]
                    ( option
                        []
                        [ text "-- Please Select --" ]
                        :: (List.map userOption <| Dict.toList userRows)
                    )
                , div
                    [ class "btn btn-success mt-2"
                    , onClick SubmitAssignUser
                    ]
                    [ text "Assign User" ]
                ]

        Just userId ->
            div
                [ class "form-group" ]
                [ label
                    []
                    [ strong [] [ text "Task Assigned" ] ]
                , p
                    []
                    [ Dict.get userId userRows
                        |> Maybe.andThen (Dict.get "name")
                        |> Maybe.withDefault "Name not found"
                        |> text
                    ]
                ]


userOption : ( Int, Dict String String ) -> Html Msg
userOption ( id, data ) =
    option
        [ onClick <| SelectAssignUser id ]
        [ Dict.get "name" data |> Maybe.withDefault "Name Not Found" |> text ]


editTaskSelect : Dict Int TaskRow -> Html Msg
editTaskSelect rows =
    div
        [ class "col-12" ]
        [ select
            [ class "form-control" ]
            (option
                []
                [ text "-- Please Select --" ]
                :: (List.map rowToOption <| Dict.toList rows)
            )
        ]


rowToOption : ( Int, TaskRow ) -> Html Msg
rowToOption ( id, { name } ) =
    option
        [ value <| String.fromInt id
        , onClick <| SelectEditTask id
        ]
        [ text name ]


tableView : Dict Int TaskRow -> Html Msg
tableView rows =
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
                        [ text "Description" ]
                    , th
                        [ class "border-top-0"]
                        [ text "Assigned User Id" ]
                    , th
                        [ class "border-top-0"]
                        [ text "Is Complete" ]
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


renderRow : (Int, TaskRow) -> Html Msg
renderRow ( id, { name, description, assignedUserId, isComplete }) =
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
            [ text description ]
        , td
            []
            [ Maybe.map String.fromInt assignedUserId |> Maybe.withDefault "Not Assigned" |> text ]
        , td
            []
            [ text <| if isComplete then "Yes" else "No" ]
        ]