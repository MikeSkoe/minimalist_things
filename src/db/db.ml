type t = Index.t

let db =
      let db = Sqlite3.db_open "base.db" in
      let _ = Thing.create_table db in
      db

module Thing = Thing

