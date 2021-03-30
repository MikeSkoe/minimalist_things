type t = Index.t

type thing_row = Thing_db.thing_row

let make db_name =
    let db = Sqlite3.db_open db_name in
    let _ = Thing_db.create_table db in
    db

let get_data = Thing_db.get_data

let get_thing = Thing_db.get_thing

let delete_thing = Thing_db.delete

let add_thing = Thing_db.add

