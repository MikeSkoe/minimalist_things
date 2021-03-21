type t = {
    things: Thing.t list;
    selected: int;
    query: bool * string;
}

let make things query = {
    selected = 0;
    things;
    query;
}

let up state = ({
    state with
    selected = max 0 (state.selected - 1);
})

let down state = ({
    state with
    selected = max 0 (min (List.length state.things - 1) (state.selected + 1))
})

