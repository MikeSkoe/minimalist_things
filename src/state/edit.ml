type field =
      | Name
      | Necessity

type msg =
      | Up
      | Down
      | UpdateField of field * string
      | Init of int option
      | SaveThing

type model = {
      name: string;
      necessity: string;
      field: field;
      someId: int option;
}

module Make(Input: Abstract.DB) = struct
      let db = Input.db

      let initial_model = {
            someId = None;
            name = "";
            necessity = "";
            field = Name;
      }


      let update_field field value state =
            if field = Name
            then { state with name = value; field = Name }
            else { state with necessity = value; field = Necessity }

      let up state = {
            state with
            field = Name;
      }

      let down state = {
            state with
            field = Necessity;
      }

      let init =
            function
            | Some id ->
                  begin match Input.Thing.get_one db id with
                  | Some item ->
                        { initial_model with
                              name = item.name;
                              necessity = item.necessity;
                              someId = Some id;
                        }
                  | None -> initial_model
                  end
            | None -> initial_model

      let save_thing state =
            begin match state.someId with
            | Some id ->
                  let _ = Input.Thing.delete db id in
                  let _ = Input.Thing.add db state.name state.necessity in
                  initial_model
            | None ->
                  let _ = Input.Thing.add db state.name state.necessity in
                  initial_model
            end

      let update msg state =
            match msg with
            | Up -> up state
            | Down -> down state
            | UpdateField (field, value) -> update_field field value state
            | Init id_opt -> init id_opt
            | SaveThing -> save_thing state
end

