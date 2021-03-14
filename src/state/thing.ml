type t = {
      id: int;
      name: string;
      necessity: string;
}

let make ~id ~name ~necessity = { id; name; necessity }

let string_of_t (selected: int) index t =
      Printf.sprintf
            "%s %s: %s"
            (if selected = index then "* " else "  ")
            t.name
            t.necessity

(* Monad? *)
let get_id things selected =
    let thing = List.(nth_opt things selected)
    in match thing with
    | Some thing -> Some thing.id
    | None -> None

let get_thing things id = List.find (fun thing -> thing.id = id) things
