open Notty

let get_messages event (state: State.View.model) =
      let open State in
      match state.confirm, state.query with
      | Some _, _ ->
            begin match event with
            | `Key (`Enter, _) -> [`View View.ApplyConfirm; `View View.(Init None)]
            | `Key (`Escape, _) -> [`View View.(Init None)]
            | _ -> [`Navigation Navigation.Nothing]
            end
      | None, (true, query) ->
            begin match event with
            | `Key (`Enter, _) -> [`View View.(Init (Some query))]
            | `Key (`Escape, _) -> [`View View.(UpdateQuery (false, ""))]
            | `Key (`ASCII chr, _) -> [`View View.(UpdateQuery (true, Input.add_char query chr))]
            | `Key (`Uchar uchr, _) -> [`View View.(UpdateQuery (true, Input.add_uchar query uchr))]
            | `Key (`Backspace, _) -> [`View View.(UpdateQuery (true, Input.backspace query))]
            | _ -> [`Navigation Navigation.Nothing]
            end
      | None, (false, query) ->
            begin match event with
            | `Key (`Escape, _) ->
                  if query = ""
                  then [`Navigation Navigation.Quit]
                  else [`View View.(Init None)]
            | `Key (`Arrow `Up, _) -> [`View View.Up]
            | `Key (`Arrow `Down, _) -> [`View View.Down]
            | `Key (`ASCII ' ', _) -> [`Navigation (Navigation.ToEdit None)]
            | `Key (`Enter, _) -> [`Navigation (Navigation.ToEdit (Thing.get_id state.things state.selected))]
            | `Key (`Backspace, _) -> (
                  try [`View (Confirm View.(DeleteThing List.(nth state.things state.selected).id))]
                  with Failure _ -> [`Navigation Navigation.ToView]
            )
            | `Key (`ASCII '/', _) -> [`View View.(UpdateQuery (true, ""))]
            | `Key (`ASCII 'q', _)
            | _ -> [`Navigation Navigation.Nothing]
            end

let draw (state: State.View.model) =
      let actions =
            match state.confirm with
            | None -> [Info.(Space, "ADD"); Info.(Enter, "EDIT"); Info.(Backspace, "DELETE"); Info.(Slash, "FIND")]
            | Some _ -> [Info.(Enter, "OK"); Info.(Escape, "CANCEL")] in
      let confirm_msg =
            match state.confirm with
            | Some (DeleteThing _) -> Some "Do you want to delete the item?"
            | None -> None in
      I.(
            Query.Img.(draw {query=state.query})
            <-> Info.Img.(draw {actions})
            <-> Confirm.Img.(draw {msg=confirm_msg})
            <-> (
                  state.things
                  |> List.mapi (fun index (thing: State.Thing.t) -> Thing.Img.draw {name=thing.name; necessity=thing.necessity; selected=(index = state.selected)})
                  |> List.fold_left I.(<->) I.empty
            )
      )

