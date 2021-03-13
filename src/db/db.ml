let make () = Sqlite3.db_open "test.db"

let get_data = Thing_db.get_data

let init db =
    let things = Thing_db.get_data db
    in State.to_view things

let add_thing db state =
    match state with
    | State.Add state ->
        let _ = Thing_db.add db state.title state.description
        in init db
    | State.View state -> State.View state
    | State.Confirm state -> State.Confirm state

let delete_thing db state id =
    match state with
    | State.Add state -> State.Add state
    | State.View state -> State.View state
    | State.Confirm _ ->
        let _ = Thing_db.delete db id
        in init db

let reducer (db: Index.t) (state, msg) =
    let open State in
    let state =
        match msg with
        | Quit -> state
        | Init -> init db
        | Up -> state
        | Down -> state
        | ToAdd -> state
        | ToView things -> State.to_view things
        | UpdateField _ -> state
        | AddThing -> add_thing db state
        | DeleteThing id -> delete_thing db state id
        | RequestConfirm _ -> state
    in (state, msg)

