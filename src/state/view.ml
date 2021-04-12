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

module Make(Input: Abstract.DB) = struct
      let db = Input.db

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

      let load_view str_opt =
            let things =
                  Input.Thing.get_data db str_opt
                  |> List.map @@ fun ({id; name; necessity}: Input.Thing.thing_row) -> Thing.make ~id ~name ~necessity
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

      let apply_confirm state =
            match state.confirm with
            | Some (DeleteThing id) ->
                  let _ = Input.Thing.delete_thing db id in
                  { state with confirm = None }
            | None -> state

      let update msg state =
            match msg with
            | Up -> up state
            | Down -> down state
            | UpdateQuery (active, str) -> update_query state (active, str) 
            | Init str_opt -> load_view str_opt
            | Confirm confirm -> request_confirm confirm state
            | ApplyConfirm -> apply_confirm state
end

