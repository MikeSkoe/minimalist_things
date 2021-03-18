let make db_name =
    let db = Sqlite3.db_open db_name in
    let _ = Thing_db.create_table db in
    db

let get_data = Thing_db.get_data

let delete_thing db state id =
    let open State in
    match state with
    | Confirm _ ->
        let _ = Thing_db.delete db id in
        let things = Thing_db.get_data db in
        View {
            things;
            selected = 0;
        }
    | Edit state -> Edit state
    | View state -> View state

let edit_thing db state someId name necessity =
    let open State in
    match state with
    | Edit _ ->
        let _ = (match someId with
            | Some id -> Thing_db.delete db id
            | None -> true
        ) in
        let _ = Thing_db.add db name necessity in
        let things = Thing_db.get_data db in
        View {
            things;
            selected = 0;
        }
    | View state -> View state
    | Confirm state -> Confirm state

let load_view db state =
    let open State in
    match state with
    | View state ->
        let things = Thing_db.get_data db in
        View {
            state with
            things;
        }
    | Edit state -> Edit state
    | Confirm state -> Confirm state

let load_edit db state =
    let open State in
    match state with
    | Edit state -> (match state.someId with
        | Some id -> (match Thing_db.get_thing db id with
            | Some thing -> Edit {
                state with
                name = thing.name;
                necessity = thing.necessity;
            }
            | None -> Edit state
        )
        | None -> Edit state
    )
    | Confirm state -> Confirm state
    | View state -> View state

let reducer (db: Index.t) (state, msg) =
    let open State in
    let state =
        match msg with
        | Db db_msg -> (match db_msg with
            | LoadView -> load_view db state
            | LoadEdit -> load_edit db state 
            | EditThing { someId; necessity; name; } -> edit_thing db state someId name necessity 
            | DeleteThing id -> delete_thing db state id
        )
        | System _ -> state
        | Navigation _ -> state
        | UI _ -> state
        | RequestConfirm _ -> state
    in (state, msg)

