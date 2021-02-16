let db = Db.Index.make ()

(* let show_things () =
      let datas = Db.Thing_db.get_data db in
      datas
            |> List.map (fun (id, name, necessity, group_id) -> Entities.Thing.make ~id ~name ~necessity ~group_id)
            |> List.fold_left (fun acc curr -> (Entities.Thing.string_of_t curr) ^ "\n" ^ acc) ""

let show_groups () =
      let datas = Db.Group_db.get_data db in
      datas
            |> List.map (fun (id, name) -> Entities.Group.make ~id ~name)
            |> List.fold_left (fun acc curr -> (Entities.Group.string_of_t curr) ^ "\n" ^ acc) ""

let add_thing () =
      let thing_name = Utils.get_input "type name of thing"
      and necessity = Utils.get_input "why do you need the thing"
      and group_id = Utils.get_input "type group id" |> int_of_string in
      match Db.Thing_db.add db thing_name necessity group_id with
            | true -> "Added new thing"
            | false -> "Failed to add new thing"

let add_group () =
      let group_name = Utils.get_input "type name of group" in
      match Db.Group_db.add db group_name with
            | true -> "Added new group"
            | false -> "Failed to add new thing"

let update_thing () =
      let thing_id = Utils.get_input "type id of thing" |> int_of_string in
      match Db.Thing_db.update
            db
            thing_id
            (fun thing_name necessity group_id ->
                  (String.uppercase_ascii thing_name, String.uppercase_ascii necessity, group_id))
            with
            | true -> "!!"
            | false -> "??"

let delete_thing () =
      let thing_id = Utils.get_input "type id of item" |> int_of_string in
      match Db.Thing_db.delete db thing_id with
            | true -> "thing deleted"
            | false -> "Failed to delete thing"

let delete_group () =
      let group_id = Utils.get_input "type id of group" |> int_of_string in
      match Db.Group_db.delete db group_id with
            | true -> "group deleted"
            | false -> "Failed to delete group" *)