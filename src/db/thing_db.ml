let get_data (db: Index.t) =
      let select_sql = "SELECT id, name, necessity FROM things" in
      let select_stmt = Sqlite3.prepare db select_sql in
      let int_of_index = Index.int_of_index select_stmt in
      let string_of_index = Index.string_of_index select_stmt in

      let rec iter datas = match Sqlite3.step select_stmt with
            | Sqlite3.Rc.ROW ->
                  let id = int_of_index 0
                  and name = string_of_index 1
                  and necessity = string_of_index 2
                  in
                  iter [State.Thing.make ~id ~name ~necessity] @ datas
            | _ -> datas
      in iter [];;

let add (db: Index.t) name necessity =
      let insert_sql = Printf.sprintf
            "INSERT INTO things(name, necessity) VALUES ('%s','%s')"
            name necessity in

      match Sqlite3.exec db insert_sql with
            | Sqlite3.Rc.OK -> true
            | _ -> false

let delete (db: Index.t) id =
      let delete_sql = Printf.sprintf
            "DELETE FROM things WHERE id = %i"
            id in

      match Sqlite3.exec db delete_sql with
            | Sqlite3.Rc.OK -> true
            | _ -> false

let update (db: Index.t) id (update: string -> string -> int -> string * string * int) =
      let select_sql = Printf.sprintf
            "SELECT name, necessity, group_id FROM things WHERE id = %i"
            id in
      let select_stmt = Sqlite3.prepare db select_sql in

      match Sqlite3.step select_stmt with
            | Sqlite3.Rc.ROW ->
                  let int_of_index index =
                        index
                              |> Sqlite3.column select_stmt
                              |> Sqlite3.Data.to_int_exn in
                  let string_of_index index =
                        index
                              |> Sqlite3.column select_stmt
                              |> Sqlite3.Data.to_string_coerce in
                  let name = string_of_index 0
                  and necessity = string_of_index 1
                  and group_id = int_of_index 2
                  in
                  (match update name necessity group_id with
                        | (name, necessity, group_id) ->
                                    print_endline (name
                                          ^ ": " ^ necessity
                                          ^ " - " ^ (string_of_int group_id));
                                    true)
            | _ -> false
