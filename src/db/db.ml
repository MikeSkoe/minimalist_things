let make () = Sqlite3.db_open "test.db"

let get_data = Thing_db.get_data

let add_thing db state =
    match state with
    | State.Add state ->
        let _ = Thing_db.add db state.title state.description
        and things = Thing_db.get_data db
        in
        State.to_view things
    | State.View state -> State.View state

let delete_thing db state =
    match state with
    | State.Add state -> State.Add state
    | State.View state ->
        let _ = Thing_db.delete db List.(nth state.things state.selected).id
        and things = Thing_db.get_data db
        in
        State.to_view things

let reducer (db: Index.t) (state, msg) =
    let open State
    in
    let state =
        match msg with
        | Quit -> state
        | Init -> state
        | Up -> state
        | Down -> state
        | ToAdd -> state
        | ToView things -> State.to_view things
        | UpdateField _ -> state
        | AddThing -> add_thing db state
        | DeleteThing -> delete_thing db state
    in (state, msg)

