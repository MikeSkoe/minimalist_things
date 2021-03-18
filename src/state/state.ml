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

type navigation_msg =
    | ToEdit of int option
    | ToView

type db_msg =
    | LoadView
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
    | View of {
        things: Thing.t list;
        selected: int;
    }
    | Edit of {
        name: string;
        necessity: string;
        field: field;
        someId: int option;
    }
    | Confirm of {
        text: string;
        confirmMsg: msg;
    }

let initial_state = View {
    things = [];
    selected = 0;
}

let up = function
    | View state -> View {
        state with
        selected = max 0 (state.selected - 1)
    }
    | Edit state -> Edit state
    | Confirm state -> Confirm state

let down = function
    | View state -> View {
        state with
        selected = min (List.length state.things - 1) (state.selected + 1)
    }
    | Edit state -> Edit state
    | Confirm state -> Confirm state

let to_confirm text confirmMsg =
    Confirm {
        text;
        confirmMsg;
    }

let to_edit state someId =
    match state with
    | Edit state -> Edit state
    | View _ -> Edit {
        someId;
        name = "";
        necessity = "";
        field = Name;
    }
    | Confirm state -> Confirm state

let can_save field name necessity =
    let name_length = name |> String.trim |> String.length
    and necessity_length = necessity |> String.trim |> String.length
    in field = Necessity
    && name_length > 0
    && necessity_length > 0

let to_view =
    View {
        things = [];
        selected = 0;
    }

let update_field state field str =
    match state with
    | Edit state ->
        if field = Name
            then Edit { state with name = str; field = Name }
            else Edit { state with necessity = str; field = Necessity }
    | View state -> View state
    | Confirm state -> Confirm state

let reducer (state, msg) =
    let state =
        match msg with
        | UI cursor_msg -> (match cursor_msg with
            | Up -> up state
            | Down -> down state
            | UpdateField (field, str) -> update_field state field str
        )
        | Navigation navigation_msg -> (match navigation_msg with
            | ToEdit someId -> to_edit state someId
            | ToView -> to_view
        )
        | RequestConfirm (text, confirmMsg) -> to_confirm text confirmMsg
        | System _ -> state
        | Db _ -> state

    and msg = match msg with
        | Navigation navigation_msg -> (match navigation_msg with
            | ToEdit _ -> Db LoadEdit
            | ToView -> Db LoadView
        )
        | UI _ -> msg
        | RequestConfirm _ -> msg
        | System _ -> msg
        | Db _ -> msg

    in (state, msg)

