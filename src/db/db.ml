let make db_name =
    let db = Sqlite3.db_open db_name in
    let _ = Thing_db.create_table db in
    db

let get_data = Thing_db.get_data

let delete_thing db id =
    let open State in
    on_confirm @@ fun _ ->
        let _ = Thing_db.delete db id in
        let things = Thing_db.get_data db None in
        View View.(make things (false, ""))

let edit_thing db someId name necessity =
    let open State in
    let open Utils in
    on_edit @@ fun _ ->
        let _ = OptionFunctor.(Thing_db.delete db <$> someId) in
        let _ = Thing_db.add db name necessity in
        let things = Thing_db.get_data db None in
        View View.(make things (false, ""))

let load_view db str_opt =
    let open State in
    on_view @@ fun state -> 
        let things = Thing_db.get_data db str_opt in
        View View.(make things state.query)

let load_edit db =
    let open State in
    let open Utils in
    on_edit @@ fun state ->
        OptionMonad.(
              state.someId >>= Thing_db.get_thing db
              |> function
              | Some thing -> Edit (Edit.make (Some thing.id) thing.name thing.necessity)
              | None -> Edit state
        )

let reducer (db: Index.t) (state, msg) =
    let open State in
    let state =
        match msg with
        | Db db_msg -> (
            match db_msg with
            | LoadView str_opt -> load_view db str_opt state
            | LoadEdit -> load_edit db state 
            | EditThing {someId; name; necessity} -> edit_thing db someId name necessity state
            | DeleteThing id -> delete_thing db id state
        )
        | Navigation _ -> state
        | UI _ -> state
        | RequestConfirm _ -> state in
    (state, msg)

