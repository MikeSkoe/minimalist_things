open Notty
open Notty_unix

let getEvent term state : State.msg =
    let open State
    in match (state, Term.event term) with
    (* View *)
    | (View _, `Key (`Arrow `Up, _))
    | (View _, `Key (`ASCII 'k', _)) ->
        Up

    | (View _, `Key (`Arrow `Down, _))
    | (View _, `Key (`ASCII 'j', _)) ->
        Down

    | (View _, `Key (`ASCII 'a', _)) ->
        ToAdd

    | (View state, `Key (`Backspace, _))
    | (View state, `Key (`ASCII 'd', _)) ->
        let text = "Are your shure, you want to delete the item?"
        and id = List.(nth state.things state.selected).id
        in
        RequestConfirm (text, DeleteThing id)

    (* Add *)
    | (Add state, `Key (`ASCII chr, _)) ->
        if state.field = Title then
            UpdateField (Title, (state.title ^ (Char.escaped chr)))
        else
            UpdateField (Description, (state.description ^ (Char.escaped chr)))

    | (Add state, `Key (`Enter, _)) ->
        if state.field = Title then
            UpdateField (Description, "")
        else
            AddThing

    | (Add state, `Key (`Backspace, _)) ->
        if state.field = Title then
            UpdateField (Title, (String.sub state.title 0 ((String.length state.title) - 1)))
        else 
            UpdateField (Description, (String.sub state.description 0 ((String.length state.description) - 1)))

    (* Confirm *)
    | (Confirm _, `Key (`Escape, _)) ->
        Init

    | (Confirm state, `Key (`Enter, _)) ->
        state.confirmMsg

    (* Quit *)
    | _ -> Quit

let img_of_things (things: State.Thing.t list) (selected: int) =
    things
    |> List.mapi (State.Thing.string_of_t selected)
    |> List.map I.(string A.empty)
    |> List.fold_left I.(<->) I.empty

let adding_view (title: string) (description: string) (field: State.field) =
      let open State
      in match field with
      | Title -> 
            let tip = I.(string A.empty "Type name: ")
            and input = I.(string A.empty title)
            and cursor = I.(char A.(bg white) ' ' 1 1)
            in
            I.(tip <-> (input <|> cursor))

      | Description ->
            let name = I.(string A.empty ("Name: " ^ title))
            and tip = I.(string A.empty "Type description: ")
            and input = I.(string A.empty description)
            and cursor = I.(char A.(bg white) ' ' 1 1)
            in
            I.(name <-> tip <-> (input <|> cursor))

let draw term state =
    let view =
        match state with
        | State.Add state -> 
            let view = adding_view state.title state.description state.field
            in view

        | State.View state -> 
            let info = I.(string A.empty "[j]-down [k]-up [a]-add item [d]-delete item")
            and divider = I.(char A.empty '-' 10 1)
            and view = img_of_things state.things state.selected
            in I.(info <-> divider <-> view)

        | State.Confirm state ->
            let info = I.(string A.empty state.text)
            and divider = I.(char A.empty ' ' 1 1)
            and buttons = I.(string A.empty "[enter]-ok [esc]-cancel")
            in I.(info <-> divider <-> buttons)

    in Term.image term view

let reducer (term: Term.t) (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

