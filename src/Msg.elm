module Msg exposing (..)


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
    | SeedEvents

