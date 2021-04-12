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

module Make(Input: Abstract.DB) = struct
      module View_Page = View.Make(Input)
      module Edit_Page = Edit.Make(Input)

      let reducer (state, msgs) =
            let state = 
                  msgs
                  |> List.fold_left (fun state msg ->
                        match msg with
                        | `Navigation msg ->
                              begin match msg with
                              | Navigation.ToEdit id_opt -> Edit Edit_Page.(update (Init id_opt) initial_model)
                              | Navigation.ToView -> View View_Page.(update (Init None) initial_model)
                              | Navigation.Nothing -> noop state
                              | Navigation.Quit -> noop state
                              end
                        | `View msg -> (on_view View_Page.(update msg)) state
                        | `Edit msg -> (on_edit Edit_Page.(update msg)) state
                  ) state in

            (state, msgs)
end

