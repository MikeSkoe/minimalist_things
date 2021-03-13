module Thing = Thing

type field =
    | Title
    | Description

type msg =
    | Quit
    | Init
    | Up
    | Down
    | ToAdd
    | ToView of Thing.t list
    | UpdateField of field * string
    | AddThing
    | DeleteThing

type t =
    | View of {
        things: Thing.t list;
        selected: int;
    }
    | Add of {
        title: string;
        description: string;
        field: field;
    }

let up = function
    | View state -> View {
        state with
        selected = max 0 (state.selected - 1)
    }
    | Add state -> Add state

let down = function
    | View state -> View {
        state with
        selected = min (List.length state.things - 1) (state.selected + 1)
    }
    | Add state -> Add state

let to_add =
    Add {
        title = "";
        description = "";
        field = Title;
    }

let to_view things =
    View {
        things;
        selected = 0;
    }

let update_field state field str =
    match state with
    | Add state ->
        if field = Title then
            Add { state with title = str; field = Title }
        else
            Add { state with description = str; field = Description }
    | View state -> View state

let reducer (state, msg) =
    let state =
        match msg with
        | Init -> state
        | Quit -> state
        | Up -> up state
        | Down -> down state
        | ToAdd -> to_add
        | ToView things -> to_view things
        | UpdateField (field, str) -> update_field state field str
        | AddThing -> state
        | DeleteThing -> state
    in (state, msg)

