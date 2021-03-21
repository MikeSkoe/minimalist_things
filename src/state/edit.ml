type field =
    | Name
    | Necessity

type t = {
    name: string;
    necessity: string;
    field: field;
    someId: int option;
}

let make someId name necessity = ({
    someId;
    name;
    necessity;
    field = Name;
})

let get_curr_field state =
    if state.field = Name
    then state.name
    else state.necessity

let update_field field str state =
    if field = Name
    then { state with name = str; field = Name }
    else { state with necessity = str; field = Necessity }

let can_save state =
    let name_length = state.name |> String.trim |> String.length
    and necessity_length = state.necessity |> String.trim |> String.length in
    state.field = Necessity
    && name_length > 0
    && necessity_length > 0

