module View exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Event exposing (Event)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Set exposing (Set)


view : Model -> Html Msg
view model =
    let
        userHeaders =
            Event.toCurrentUserHeaders model.position model.events
                |> Set.toList

        userRows =
            Event.toCurrentUserRows model.position model.events
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
                [ div
                    [ class "form-group" ]
                    [ label
                        []
                        [ text "Create User" ]
                    , input
                        [ type_ "text"
                        , value model.createUser
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
                ]
            , div
                [ class "col-7 list-group mt-4" ]
                (div
                    [ class "list-group-item list-group-item-primary" ]
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
            [ class "row mt-2" ]
            [ div
                [ class "col-12" ]
                [ h4
                    []
                    [ text "Edit User" ]
                ]
            , model.editingUser
                |> Maybe.andThen (getUser userRows)
                |> Maybe.map (renderEdit model.editingUserData)
                |> Maybe.withDefault (editUserSelect userRows)
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


getUser : Dict Int (Dict String String) -> Int -> Maybe (Dict String String)
getUser rows id =
    Dict.get id rows


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
                [ class "col-3" ]
                [ div
                    [ class "btn btn-danger"
                    , onClick CancelEditing
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
                            [ class "offset-6 col-3" ]
                            [ div
                                [ class "btn btn-info"
                                , onClick AddEditingRow
                                ]
                                [ text "Add Row" ]
                            ]
                        , div
                            [ class "col-3" ]
                            [ div
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
        [ class "col-12" ]
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


rowToOption : ( Int, Dict String String ) -> Html Msg
rowToOption ( id, data ) =
    option
        -- @todo - should probably do this properly with a decoder,
        --         but this will work for now.
        [ value <| String.fromInt id
        , onClick <| SelectEditUser id
        ]
        [ Dict.get "name" data |> Maybe.withDefault "Name Not Found" |> text ]


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
        [ class "list-group-item" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-8" ]
                [ p
                    [ class "bold" ]
                    [ text <| Event.toName event ]
                ]
            , div
                [ class "col-4" ]
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
