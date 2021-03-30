type confirm =
      | DeleteThing of int

type msg =
      | Up
      | Down
      | UpdateQuery of bool * string
      | Init of string option
      | Confirm of confirm
      | ApplyConfirm

type model = {
      things: Thing.t list;
      selected: int;
      query: bool * string;
      confirm: confirm option;
}

let initial_model = {
      selected = 0;
      things = [];
      query = (false, "");
      confirm = None;
}

let up state = ({
      state with
      selected = max 0 (state.selected - 1);
})

let down state = ({
      state with
      selected = max 0 (min (List.length state.things - 1) (state.selected + 1))
})

let update_query state query = ({
      state with
      query
})

let load_view db str_opt =
      let things =
            Db.get_data db str_opt
            |> List.map @@ fun ({id; name; necessity}: Db.thing_row) -> Thing.make ~id ~name ~necessity
      in
      { initial_model with
      things;
      query =
            match str_opt with
            | Some str -> (false, str)
            | None -> (false, "")
      }

let request_confirm confirm state =
      { state with confirm = Some confirm }

let apply_confirm db state =
      match state.confirm with
      | Some (DeleteThing id) ->
            let _ = Db.delete_thing db id in
            { state with confirm = None }
      | None -> state

let update db msg state =
      match msg with
      | Up -> up state
      | Down -> down state
      | UpdateQuery (active, str) -> update_query state (active, str) 
      | Init str_opt -> load_view db str_opt
      | Confirm confirm -> request_confirm confirm state
      | ApplyConfirm -> apply_confirm db state

