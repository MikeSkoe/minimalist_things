open Utils

module Thing = Thing
module View = View
module Edit = Edit
module Confirm = Confirm

type system_msg =
    | Quit
    | Nothing

type ui_msg =
    | Up
    | Down
    | UpdateField of Edit.field * string
    | UpdateQuery of bool * string

type navigation_msg =
    | ToEdit of int option
    | ToView

type db_msg =
    | LoadView of string option
    | LoadEdit 
    | EditThing of {
        name: string;
        necessity: string;
        someId: int option;
    }
    | DeleteThing of int

type msg =
    | System of system_msg
    | Navigation of navigation_msg
    | UI of ui_msg
    | Db of db_msg
    | RequestConfirm of string * msg

type t =
    | View of View.t
    | Edit of Edit.t
    | Confirm of msg Confirm.t

let on_view fn = function
    | View state -> fn state
    | Edit state -> Edit state
    | Confirm state -> Confirm state

let on_edit fn = function
    | View state -> View state
    | Edit state -> fn state
    | Confirm state -> Confirm state

let on_confirm fn = function
    | View state -> View state
    | Edit state -> Edit state
    | Confirm state -> fn state

let to_edit someId = on_view $ fun _ ->
    Edit Edit.(make someId "" "")

let update_query query = on_view $ fun state ->
    View View.(make state.things query)

let update_field field str = on_edit $ fun state ->
    Edit Edit.(update_field field str state)

let up = on_view $ fun state ->
    View View.(up state)

let down = on_view $ fun state ->
    View View.(down state)

let reducer (state, msg) =
    let state =
        match msg with
        | UI cursor_msg -> (
            match cursor_msg with
            | Up -> up state
            | Down -> down state
            | UpdateField (field, str) -> update_field field str state
            | UpdateQuery (active, str) -> update_query (active, str) state
        )
        | Navigation navigation_msg -> (
            match navigation_msg with
            | ToEdit someId -> to_edit someId state
            | ToView -> View View.(make [] (false, ""))
        )
        | RequestConfirm (text, confirmMsg) -> Confirm Confirm.(make text confirmMsg)
        | System _ -> state
        | Db _ -> state

    and msg =
        match msg with
        | Navigation navigation_msg -> (
            match navigation_msg with
            | ToEdit _ -> Db LoadEdit
            | ToView -> Db (LoadView None)
        )
        | UI ui_msg -> (
            match ui_msg with
            | Up -> msg
            | Down -> msg
            | UpdateField _ -> msg
            | UpdateQuery (false, "") -> Db (LoadView None)
            | UpdateQuery (false, str) -> Db (LoadView (Some str))
            | UpdateQuery _ -> msg
        )
        | RequestConfirm _ -> msg
        | System _ -> msg
        | Db _ -> msg

    in (state, msg)

