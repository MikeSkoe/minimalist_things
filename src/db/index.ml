open Utils

let make () = Sqlite3.db_open "test.db"

type t = Sqlite3.db

let int_of_index stmt
      = Sqlite3.column stmt
      >> Sqlite3.Data.to_int_exn

let string_of_index stmt
      = Sqlite3.column stmt
      >> Sqlite3.Data.to_string_coerce
