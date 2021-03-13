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