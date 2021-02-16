type add_input = string

type view_type =
      | View
      | Add of add_input

type t = {
      things: Thing.t list;
      selected: int;
      view_type: view_type;
}