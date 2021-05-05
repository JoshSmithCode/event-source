module Event exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)


type Event
    = CreateUser String
    | UpdateUser Int (Dict String String)
    | CreateTask String
    | UpdateTaskDescription Int String
    | AssignTask Int Int
    | CompleteTask Int


toName : Event -> String
toName event =
    case event of
        CreateUser _ -> "Create User"

        UpdateUser _ _ -> "Update User"

        CreateTask _ ->  "Create Task"

        UpdateTaskDescription _ _ -> "Update Task Description"

        AssignTask _ _ -> "Assign Task"

        CompleteTask _ -> "Complete Task"


renderBody : Event -> Html msg
renderBody event =
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
                [ tbody
                    []
                    [ th
                        []
                        [ text "User ID" ]
                    , th
                        []
                        [ text <| String.fromInt id ]
                    ]
                , headings
                , tbody
                    []
                    (List.map updateBody <| Dict.toList body)
                ]

        CreateTask name ->
            div
                [ class "row" ]
                [ div
                    [ class "col-6" ]
                    [ text "name" ]
                , div
                    [ class "col-6" ]
                    [ text name ]
                ]

        UpdateTaskDescription id description ->
            div
                [ class "row" ]
                [ div
                    [ class "col-6" ]
                    [ text "Task ID" ]
                , div
                    [ class "col-6" ]
                    [ text <| String.fromInt id ]
                , div
                    [ class "col-6" ]
                    [ text "name" ]
                , div
                    [ class "col-6" ]
                    [ text description ]
                ]

        AssignTask taskId userId ->
            div
                [ class "row" ]
                [ div
                    [ class "col-6" ]
                    [ text "Task ID" ]
                , div
                    [ class "col-6" ]
                    [ text <| String.fromInt taskId ]
                , div
                    [ class "col-6" ]
                    [ text "User ID" ]
                , div
                    [ class "col-6" ]
                    [ text <| String.fromInt userId ]
                ]

        CompleteTask id ->
            div
                [ class "row" ]
                [ div
                    [ class "col-6" ]
                    [ text "Task ID" ]
                , div
                    [ class "col-6" ]
                    [ text <| String.fromInt id ]
                ]

headings : Html msg
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


updateBody : ( String, String ) -> Html msg
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


toCurrentUserHeaders : Int -> Array Event -> Set String
toCurrentUserHeaders position events =
    events
        |> Array.slice 0 position
        |> Array.toList
        |> List.foldl toUserHeaders Set.empty


toUserHeaders : Event -> Set String -> Set String
toUserHeaders event headers =
    case event of
        CreateUser _ ->
            Set.insert "name" headers

        UpdateUser _ data ->
            data
                |> Dict.keys
                |> List.foldl Set.insert headers

        _ ->
            headers


toCurrentUserRows : Int -> Array Event -> Dict Int (Dict String String)
toCurrentUserRows position events =
    events
        |> Array.slice 0 position
        |> Array.toIndexedList
        |> List.foldl toUserRows Dict.empty


toUserRows : ( Int, Event ) -> Dict Int (Dict String String) -> Dict Int (Dict String String)
toUserRows ( index, event ) rows =
    case event of
        CreateUser name ->
            Dict.insert index (Dict.singleton "name" name) rows

        UpdateUser id data ->
            Dict.get id rows
                |> Maybe.withDefault Dict.empty
                |> Dict.union data
                |> (\updated -> Dict.insert id updated rows)

        _ ->
            rows


type alias TaskRow =
    { name : String
    , description : String
    , assignedUserId : Maybe Int
    , isComplete : Bool
    }


toCurrentTaskRows : Int -> Array Event -> Dict Int TaskRow
toCurrentTaskRows position events =
    events
        |> Array.slice 0 position
        |> Array.toIndexedList
        |> List.foldl toTaskRows Dict.empty


toTaskRows : ( Int, Event ) -> Dict Int TaskRow -> Dict Int TaskRow
toTaskRows ( index, event ) rows =
    case event of
        CreateTask name ->
            Dict.insert index (TaskRow name "" Nothing False) rows

        UpdateTaskDescription id description ->
            Dict.update id (Maybe.map (\task -> { task | description = description})) rows

        AssignTask taskId userId ->
            Dict.update taskId (Maybe.map (\task -> { task | assignedUserId = Just userId })) rows

        CompleteTask id ->
            Dict.update id (Maybe.map (\task -> { task | isComplete = True })) rows

        _ ->
            rows


seedEvents : Array Event
seedEvents =
    let
        data =
            """[{"CreateUser":"Josh Smith"},{"UpdateUser":{"id":0,"fields":{"name":"Joshua Smith","email":"josh@test.dev"}}},{"CreateUser":"Jessica Cooper"},{"CreateUser":"Tom Foolery"},{"CreateUser":"Harry"},{"UpdateUser":{"id":4,"fields":{"name":"Harry Lowe","email":"harry@dev.io"}}},{"UpdateUser":{"id":3,"fields":{"email":"tom@haha.lol"}}},{"UpdateUser":{"id":2,"fields":{"email":"jessica@employee.dev"}}},{"CreateTask":"Create login page"},{"CreateTask":"Tom and Bob reunion party"},{"CreateTask":"Buy fancier pants"},{"AssignTask":{"taskId":8,"userId":4}},{"AssignTask":{"taskId":3,"userId":10}},{"UpdateTaskDescription":{"id":9,"description":"Bob has agreed that no eyes will be consumed as long as the doors are opened."}},{"CompleteTask":10},{"CreateTask":"Chiron Beta Prime zoom conference"},{"UpdateTaskDescription":{"id":3,"description":"Don't forget to send them their xmas hamper 9-10 months in advance"}},{"AssignTask":{"taskId":15,"userId":4}}]"""
    in
    case Decode.decodeString (Decode.array decoder) data of
        Err error ->
            Debug.todo (Decode.errorToString error)

        Ok array ->
            array



decoder : Decoder Event
decoder =
    Decode.oneOf
        [ createUserDecoder
        , updateUserDecoder
        , createTaskDecoder
        , updateTaskDescriptionDecoder
        , assignTaskDecoder
        , completeTaskDecoder
        ]


createUserDecoder : Decoder Event
createUserDecoder =
    Decode.field "CreateUser" Decode.string
        |> Decode.map CreateUser


updateUserDecoder : Decoder Event
updateUserDecoder =
    Decode.field "UpdateUser"
        <| Decode.map2 UpdateUser
            (Decode.field "id" Decode.int)
            (Decode.field "fields" <| Decode.dict Decode.string)


createTaskDecoder : Decoder Event
createTaskDecoder =
    Decode.field "CreateTask" Decode.string
        |> Decode.map CreateTask


updateTaskDescriptionDecoder : Decoder Event
updateTaskDescriptionDecoder =
    Decode.field "UpdateTaskDescription"
        <| Decode.map2 UpdateTaskDescription
            (Decode.field "id" Decode.int)
            (Decode.field "description" Decode.string)


assignTaskDecoder : Decoder Event
assignTaskDecoder =
    Decode.field "AssignTask"
        <| Decode.map2 AssignTask
            (Decode.field "taskId" Decode.int)
            (Decode.field "userId" Decode.int)


completeTaskDecoder : Decoder Event
completeTaskDecoder =
    Decode.field "CompleteTask" Decode.int
        |> Decode.map CompleteTask