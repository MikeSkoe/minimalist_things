module Thing = Thing
module View = View
module Edit = Edit
module Navigation = Navigation

type msg =
      [ `Navigation of Navigation.msg
      | `View of View.msg
      | `Edit of Edit.msg
      ]

type model =
      | View of View.model
      | Edit of Edit.model

let on_view fn =
      function
      | View state -> View (fn state)
      | Edit state -> Edit state

let on_edit fn =
      function
      | View state -> View state
      | Edit state -> Edit (fn state)

let noop a = a

let reducer db (state, msgs) =
      let state = 
            msgs
            |> List.fold_left (fun state msg ->
                  match msg with
                  | `Navigation msg ->
                        begin match msg with
                        | Navigation.ToEdit id_opt -> Edit Edit.(update db (Init id_opt) initial_model)
                        | Navigation.ToView -> View View.(update db (Init None) initial_model)
                        | Navigation.Nothing -> noop state
                        | Navigation.Quit -> noop state
                        end
                  | `View msg -> (on_view View.(update db msg)) state
                  | `Edit msg -> (on_edit Edit.(update db msg)) state
            ) state in

      (state, msgs)

