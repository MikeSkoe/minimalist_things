let (>>) f1 f2 arg = arg |> f1 |> f2

let get_input name =
      print_endline name;
      read_line ()
