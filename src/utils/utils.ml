let (>>) f1 f2 arg = arg |> f1 |> f2
let (<<) f1 f2 arg = arg |> f2 |> f1

let ($) f arg = f arg

let get_input name =
      print_endline name;
      read_line ()
