module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)


type alias Model =
    { userEvents : Array Event
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
    { userEvents = Array.empty
    , createUser = ""
    , createEventData = Dict.empty
    , createEventUser = Nothing
    , position = 0
    , expanded = Set.empty
    , editingUser = Nothing
    , editingUserData = Array.empty
    }


type Event
    = CreateUser String
    | UpdateUser Int (Dict String String)


eventToName : Event -> String
eventToName event =
    case event of
        CreateUser _ ->
            "Create User"

        UpdateUser _ _ ->
            "Update User"


type Msg
    = UpdateCreateUser String
    | SubmitCreateUser
    | Expand Int
    | Collapse Int
    | SelectEditUser Int
    | CancelEditing
    | AddEditingRow 
    | UpdateEditingRowKey Int String
    | UpdateEditingRowValue Int String
    | SubmitEditUser
    | UpdatePosition String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCreateUser string ->
            { model | createUser = string }

        SubmitCreateUser ->
            { model
                | userEvents =
                    Array.push
                        (CreateUser model.createUser)
                        model.userEvents
                , createUser = ""
                , position = Array.length model.userEvents + 1
            }

        Expand int ->
            { model | expanded = Set.insert int model.expanded }

        Collapse int ->
            { model | expanded = Set.remove int model.expanded }

        SelectEditUser int ->
            case toRows model.position model.userEvents |> Dict.get int of 
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
                            |> UpdateUser index
                            
                    in
                    { model 
                        | userEvents = Array.push newEvent model.userEvents
                        , editingUser = Nothing
                        , editingUserData = Array.empty
                        , position = Array.length model.userEvents + 1
                    }
                    
        UpdatePosition value ->
            case String.toInt value of 
                Nothing ->
                    model
                    
                Just position ->
                    { model | position = position }
            
            
view : Model -> Html Msg
view model =
    let
        headers =
            toHeaders model.position model.userEvents
                |> Set.toList

        rows =
            toRows model.position model.userEvents
    in
    div
        [ class "container" ]
        [ div
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
                    :: (Array.indexedMap (showEvent model.expanded) model.userEvents |> Array.toList)
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
                    , Attributes.max <| String.fromInt <| Array.length model.userEvents
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
                |> Maybe.andThen (getUser rows)
                |> Maybe.map (renderEdit model.editingUserData)
                |> Maybe.withDefault (editUserSelect rows)       
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
                            :: List.map (\heading -> th [] [ text heading ]) headers
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



-- @todo - Realistically instead of events being stored in an Array
--         I could define a better data element with an internal pointer
--         to save the need to do stuff like this. BUT, this will suffice for now
toHeaders : Int -> Array Event -> Set String
toHeaders position events =
    events
        |> Array.slice 0 position
        |> Array.toList 
        |> List.foldl eventToHeaders Set.empty


toRows : Int -> Array Event -> Dict Int (Dict String String)
toRows position events =
    events
        |> Array.slice 0 position
        |> Array.toIndexedList
        |> List.foldl eventToRows Dict.empty


eventToHeaders : Event -> Set String -> Set String
eventToHeaders event headers =
    case event of
        CreateUser _ ->
            Set.insert "name" headers

        UpdateUser _ data ->
            data
                |> Dict.keys
                |> List.foldl Set.insert headers


eventToRows : ( Int, Event ) -> Dict Int (Dict String String) -> Dict Int (Dict String String)
eventToRows ( index, event ) rows =
    case event of
        CreateUser name ->
            Dict.insert index (Dict.singleton "name" name) rows

        UpdateUser id data ->
            Dict.get id rows
                |> Maybe.withDefault Dict.empty
                |> Dict.union data
                |> (\updated -> Dict.insert id updated rows)


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
                    [ text <| eventToName event ]
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
            eventBody event

          else
            text ""
        ]


eventBody : Event -> Html Msg
eventBody event =
    case event of
        CreateUser name ->
            div
                [ class "row" ]
                [ div
                    [ class "col-6" ]
                    [ text "name" ]
                , div
                    [ class "col-6" ]
                    [ text name ]
                ]

        UpdateUser id body ->
            table
                [ class "table" ]
                [ headings
                , tbody
                    []
                    (List.map updateBody <| Dict.toList body)
                ]


headings : Html Msg
headings =
    thead
        []
        [ th
            []
            [ text "Field" ]
        , th
            []
            [ text "Value" ]
        ]


updateBody : ( String, String ) -> Html Msg
updateBody ( field, value ) =
    tr
        []
        [ td
            []
            [ text field ]
        , td
            []
            [ text value ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
