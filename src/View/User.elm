module View.User exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msg exposing (Msg(..))


createForm : String -> Html Msg
createForm createUserValue =
     div
         [ class "form-group" ]
         [ label
             []
             [ text "Create User" ]
         , input
             [ type_ "text"
             , value createUserValue
             , onInput UpdateCreateUser
             , class "form-control"
             ]
             []
         , div
             [ class "btn btn-primary mt-1"
             , onClick SubmitCreateUser
             ]
             [ text "Add User" ]
         ]


editForm : Dict Int (Dict String String) -> Maybe Int -> Array ( String, String ) -> Html Msg
editForm rows maybeUserId editingUserData =
    div
        [ class "row mt-2" ]
        [ div
            [ class "col-12" ]
            [ h4
                []
                [ text "Edit User" ]
            ]
        , maybeUserId
            |> Maybe.andThen (\id -> Dict.get id rows)
            |> Maybe.map (renderEdit editingUserData)
            |> Maybe.withDefault (editUserSelect rows)
        ]


editUserSelect : Dict Int (Dict String String) -> Html Msg
editUserSelect rows =
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


rowToOption : ( Int, Dict String String ) -> Html Msg
rowToOption ( id, data ) =
    option
        -- @todo - should probably do this properly with a decoder,
        --         but this will work for now.
        [ onClick <| SelectEditUser id ]
        [ Dict.get "name" data |> Maybe.withDefault "Name Not Found" |> text ]


renderEdit : Array (String, String) -> Dict String String -> Html Msg
renderEdit editingData row =
    div
        [ class "col-12" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-9" ]
                [ Dict.get "name" row |> Maybe.withDefault "Name not found" |> text ]
            , div
                [ class "col-3 d-flex justify-content-end" ]
                [ div
                    [ class "btn btn-danger"
                    , onClick CancelEditingUser
                    ]
                    [ text "Cancel" ]
                ]
            ]
        , div
            [ class "row" ]
            ( div
                [ class "col-12" ]
                [ div
                    [ class "row" ]
                    [ div
                        [ class "col-6" ]
                        [ text "Key" ]
                    , div
                        [ class "col-6" ]
                        [ text "Value" ]
                    ]
                ]
            :: List.map renderEditInputs (Array.toIndexedList editingData)
            ++ [ div
                    [ class "col-12" ]
                    [ div
                        [ class "row" ]
                        [ div
                            [ class "offset-6 col-6 d-flex justify-content-end mt-2" ]
                            [ div
                                [ class "btn btn-info mr-2"
                                , onClick AddEditingRow
                                ]
                                [ text "Add Row" ]
                            , div
                                [ class "btn btn-success"
                                , onClick SubmitEditUser
                                ]
                                [ text "Save" ]
                            ]
                        ]
                    ]
               ]
            )
        ]


renderEditInputs : (Int, (String, String)) -> Html Msg
renderEditInputs (index, (key, val)) =
    div
        [ class "col-12 mt-2" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-6" ]
                [ input
                    [ class "form-control"
                    , type_ "text"
                    , onInput (UpdateEditingRowKey index)
                    , value key
                    ]
                    []
                ]
            , div
                [ class "col-6" ]
                [ input
                    [ class "form-control"
                    , type_ "text"
                    , onInput (UpdateEditingRowValue index)
                    , value val
                    ]
                    []
                ]
            ]
        ]


tableView : List String -> Dict Int (Dict String String) -> Html Msg
tableView headers rows =
    div
        [ class "row" ]
        [ div
            [ class "col-12" ]
            [ table
                [ class "table table-bordered table-sm border-top-0" ]
                [ thead
                    []
                    (th
                        [ class "border-top-0" ]
                        [ text "ID" ]
                        :: List.map (\heading -> th [ class "border-top-0" ] [ text heading ]) headers
                    )
                , tbody
                    []
                    (rows
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (renderRow headers)
                    )
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