let (>>) f1 f2 arg = arg |> f1 |> f2

let get_input title =
      print_endline title;
      read_line ()
