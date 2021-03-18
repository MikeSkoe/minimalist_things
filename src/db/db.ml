let make db_name =
    let db = Sqlite3.db_open db_name in
    let _ = Thing_db.create_table db in

    db

let get_data = Thing_db.get_data

let init db =
    let things = Thing_db.get_data db
    in State.to_view things

let add_thing db state =
    match state with
    | State.Edit state ->
        let _ =
            (match state.someId with
            | Some id -> Thing_db.delete db id
            | None -> true
            )in
        let _ = Thing_db.add db state.name state.necessity
        in init db

    | State.View state -> State.View state
    | State.Confirm state -> State.Confirm state

let delete_thing db state id =
    match state with
    | State.Edit state -> State.Edit state
    | State.View state -> State.View state
    | State.Confirm _ ->
        let _ = Thing_db.delete db id
        in init db

let reducer (db: Index.t) (state, msg) =
    let open State in
    let state =
        match msg with
        | System system_msg -> (match system_msg with
            | Quit -> state
            | Init -> init db
        )
        | UI _ -> state
        | Navigation navigation_msg -> (match navigation_msg with
            | ToEdit _ -> state
            | ToView things -> State.to_view things
        )
        | Db db_msg -> (match db_msg with
            | EditThing -> add_thing db state
            | DeleteThing id -> delete_thing db state id
        )
        | Nothing -> state
        | RequestConfirm _ -> state
    in (state, msg)

