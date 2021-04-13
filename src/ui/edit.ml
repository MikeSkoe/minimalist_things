open Notty
open State

let get_curr_field (state: State.Edit.model) =
      if state.field = Name
      then state.name
      else state.necessity

let can_save (state: State.Edit.model) =
      let name_length = state.name |> String.trim |> String.length in
      let necessity_length = state.necessity |> String.trim |> String.length in
      name_length > 0 && necessity_length > 0

let get_messages event state =
      match event with
      | `Key (`Arrow `Up, _) -> [`Edit Edit.(UpdateField (Name, state.name))]
      | `Key (`Arrow `Down, _) -> [`Edit Edit.(UpdateField (Necessity, state.necessity))]

      | `Key (`Enter, _) ->
            if can_save state
            then [`Edit Edit.SaveThing; `Navigation Navigation.ToView]
            else [`Edit Down]

      | `Key (`Uchar uchr, _) ->
            let str = get_curr_field state in
            [`Edit (UpdateField (state.field, Input.add_uchar str uchr))]

      | `Key (`ASCII chr, _) ->
            let str = get_curr_field state in
            [`Edit (UpdateField (state.field, Input.add_char str chr))]

      | `Key (`Backspace, _) ->
            let str = get_curr_field state in
            [`Edit (UpdateField (state.field, Input.backspace str))]

      | `Key (`Escape, _) -> [`Navigation State.Navigation.ToView]
      | _ -> [`Navigation State.Navigation.Nothing]

let draw state =
      let actions = 
            if can_save state
            then [(Info.Enter, "Save"); (Info.Escape, "CANCEL")]
            else [(Info.Escape, "CANCEL")] in
      I.(
            Info.Img.(draw {actions})
            <-> Input.Img.(draw {
                  label="Name     ";
                  value=state.name;
                  active=(state.field=Name)
            })
            <-> Input.Img.(draw {
                  label="Necessity";
                  value=state.necessity;
                  active=(state.field=Necessity)
            })
      )

