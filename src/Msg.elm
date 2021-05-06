module Msg exposing (..)

import Model exposing (EditTab, Table)


type Msg
    = UpdateCreateUser String
    | SubmitCreateUser
    | UpdateCreateTask String
    | SubmitCreateTask
    | Expand Int
    | Collapse Int
    | SetEditTab EditTab
    | SelectEditUser Int
    | SelectEditTask Int
    | CancelEditingUser
    | CancelEditingTask
    | AddEditingRow
    | UpdateEditingRowKey Int String
    | UpdateEditingRowValue Int String
    | SubmitEditUser
    | UpdateEditingTaskDescription String
    | SubmitEditingTaskDescription
    | SelectAssignUser Int
    | SubmitAssignUser
    | UpdatePosition String
    | SeedEvents
    | SetTable Table

