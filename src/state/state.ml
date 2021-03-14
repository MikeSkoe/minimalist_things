module Thing = Thing

type field =
    | Name
    | Necessity

type msg =
    | Quit
    | Init
    | Nothing
    | Up
    | Down
    | ToEdit of int option
    | ToView of Thing.t list
    | UpdateField of field * string
    | EditThing
    | DeleteThing of int
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
    | View state ->
        (match someId with
        | Some id ->
            let thing = Thing.get_thing state.things id
            in Edit {
                name = thing.name;
                necessity = thing.necessity;
                someId;
                field = Name;
            }
        | None -> Edit {
            name = "";
            necessity = "";
            someId;
            field = Name;
        }
        )
    | Confirm state -> Confirm state

let can_save field name necessity =
    let name_length = name |> String.trim |> String.length
    and necessity_length = necessity |> String.trim |> String.length
    in field = Necessity
    && name_length > 0
    && necessity_length > 0

let to_view things =
    View {
        things;
        selected = 0;
    }

let update_field state field str =
    match state with
    | Edit state ->
        if field = Name then
            Edit { state with name = str; field = Name }
        else
            Edit { state with necessity = str; field = Necessity }
    | View state -> View state
    | Confirm state -> Confirm state

let reducer (state, msg) =
    let state =
        match msg with
        | Init -> state
        | Quit -> state
        | Up -> up state
        | Nothing -> state
        | Down -> down state
        | ToEdit someId -> to_edit state someId
        | ToView things -> to_view things
        | UpdateField (field, str) -> update_field state field str
        | EditThing -> state
        | DeleteThing _ -> state
        | RequestConfirm (text, confirmMsg) -> to_confirm text confirmMsg
    in (state, msg)

