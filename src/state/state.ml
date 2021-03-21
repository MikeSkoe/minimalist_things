open Utils

module Thing = Thing

type field =
    | Name
    | Necessity

type system_msg =
    | Quit
    | Nothing

type ui_msg =
    | Up
    | Down
    | UpdateField of field * string
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

type view_state = {
    things: Thing.t list;
    selected: int;
    query: bool * string;
}

type edit_state = {
    name: string;
    necessity: string;
    field: field;
    someId: int option;
}

type confirm_state = {
    text: string;
    confirmMsg: msg;
}

type t =
    | View of view_state
    | Edit of edit_state
    | Confirm of confirm_state

let get_curr_field (state: edit_state) = if state.field = Name then state.name else state.necessity

let on_view fn = function
    | View { things; selected; query } -> fn { things; selected; query }
    | Edit state -> Edit state
    | Confirm state -> Confirm state

let on_edit fn = function
    | View state -> View state
    | Edit { name; necessity; field; someId } -> fn { name; necessity; field; someId }
    | Confirm state -> Confirm state

let on_confirm fn = function
    | View state -> View state
    | Edit state -> Edit state
    | Confirm { text; confirmMsg } -> fn { text; confirmMsg }

let initial_state = View {
    things = [];
    selected = 0;
    query = (false, "");
}

let up = on_view $ fun state ->
    View {
        state with
        selected = max 0 (state.selected - 1);
    }

let down = on_view $ fun state ->
    View {
        state with
        selected = max 0 (min (List.length state.things - 1) (state.selected + 1))
    }

let to_confirm text confirmMsg =
    Confirm {
        text;
        confirmMsg;
    }

let to_edit someId = on_view $ fun _ ->
    Edit {
        someId;
        name = "";
        necessity = "";
        field = Name;
    }

let can_save edit_state =
    let name_length = edit_state.name |> String.trim |> String.length
    and necessity_length = edit_state.necessity |> String.trim |> String.length in
    edit_state.field = Necessity
    && name_length > 0
    && necessity_length > 0

let update_field field str = on_edit $ fun state ->
    if field = Name
    then Edit { state with name = str; field = Name }
    else Edit { state with necessity = str; field = Necessity }

let update_query isQuerying str = on_view $ fun state ->
    View {
        state with
        query = (isQuerying, str);
        selected = 0;
    }

let reducer (state, msg) =
    let state =
        match msg with
        | UI cursor_msg -> (match cursor_msg with
            | Up -> up state
            | Down -> down state
            | UpdateField (field, str) -> update_field field str state
            | UpdateQuery (isQuerying, str) -> update_query isQuerying str state
        )
        | Navigation navigation_msg -> (match navigation_msg with
            | ToEdit someId -> to_edit someId state
            | ToView -> initial_state
        )
        | RequestConfirm (text, confirmMsg) -> to_confirm text confirmMsg
        | System _ -> state
        | Db _ -> state

    and msg = match msg with
        | Navigation navigation_msg -> (match navigation_msg with
            | ToEdit _ -> Db LoadEdit
            | ToView -> Db (LoadView None)
        )
        | UI ui_msg -> (match ui_msg with
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

