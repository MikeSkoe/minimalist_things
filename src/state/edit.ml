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

let init db =
      function
      | Some id ->
            begin match Db.get_thing db id with
            | Some item ->
                  { initial_model with
                        name = item.name;
                        necessity = item.necessity;
                        someId = Some id;
                  }
            | None -> initial_model
            end
      | None -> initial_model

let save_thing db state =
      begin match state.someId with
      | Some id ->
            let _ = Db.delete_thing db id in
            let _ = Db.add_thing db state.name state.necessity in
            initial_model
      | None ->
            let _ = Db.add_thing db state.name state.necessity in
            initial_model
      end


let update db msg state =
      match msg with
      | Up -> up state
      | Down -> down state
      | UpdateField (field, value) -> update_field field value state
      | Init id_opt -> init db id_opt
      | SaveThing -> save_thing db state

