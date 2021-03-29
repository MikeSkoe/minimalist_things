open Notty
open Notty_unix

let list_view (state: State.View.t) =
    let actions = [(Info.Space, "ADD"); (Info.Enter, "EDIT"); (Info.Backspace, "DELETE"); (Info.Slash, "FIND")]
    in
    I.(
        Query.Img.(draw {query=state.query})
        <-> Info.Img.(draw {actions})
        <-> (
            state.things
            |> List.mapi (fun index thing -> Thing.Img.draw {thing; selected=(index = state.selected)})
            |> List.fold_left I.(<->) I.empty
        )
    )

let edit_view (state: State.Edit.t) =
    let actions = 
        if State.Edit.can_save state
        then [(Info.Enter, "Save"); (Info.Escape, "CANCEL")]
        else [(Info.Escape, "CANCEL")]
    in
    I.(
        Info.Img.(draw {actions})
        <-> Input.Img.(
            draw
            {
                label="Name     ";
                value=state.name;
                active=(state.field = State.Edit.Name)
            }
        )
        <-> Input.Img.(
            draw
            {
                label="Necessity";
                value=state.necessity;
                active=(state.field = State.Edit.Necessity)
            }
        )
    )

let confirm_view (state: State.msg State.Confirm.t) =
    let actions = [(Info.Enter, "OK"); (Info.Escape, "CANCEL")]
    in
    I.(
        Info.Img.(draw {actions})
        <-> I.(string A.empty state.text)
    )

let draw term state =
    let open State in
    let view =
        match state with
        | Edit state -> edit_view state
        | View state -> list_view state
        | Confirm state -> confirm_view state in
    Term.image term view

let getEvent term state =
    let open State in
    let event = Term.event term
    in match state with
    | View state -> (
        match state.query with
            | (true, query) -> (
                match event with
                | `Key (`Enter, _) -> UI (UpdateQuery (false, query))
                | `Key (`Escape, _) -> UI (UpdateQuery (false, ""))
                | `Key (`ASCII chr, _) -> UI (UpdateQuery (true, Input.add_char query chr))
                | `Key (`Uchar uchr, _) -> UI (UpdateQuery (true, Input.add_uchar query uchr))
                | `Key (`Backspace, _) -> UI (UpdateQuery (true, Input.backspace query))
                | _ -> Navigation Nothing
            )
            | (false, query) -> (
                match event with
                | `Key (`Escape, _) ->
                    if query = ""
                    then Navigation Quit
                    else UI (UpdateQuery (false, ""))

                | `Key (`Arrow `Up, _) -> Navigation Up
                | `Key (`Arrow `Down, _) -> Navigation Down
                | `Key (`ASCII ' ', _) -> Navigation (ToEdit None)
                | `Key (`Enter, _) -> Navigation (ToEdit (Thing.get_id state.things state.selected))
                | `Key (`Backspace, _) -> (
                    try RequestConfirm (
                        "Are your shure, you want to delete the item?",
                        Db (DeleteThing List.(nth state.things state.selected).id)
                    )
                    with Failure _ -> Navigation Nothing
                )
                | `Key (`ASCII '/', _) -> UI (UpdateQuery (true, ""))
                | `Key (`ASCII 'q', _)
                | _ -> Navigation Nothing
            )
    )

    | Confirm state -> (
        match event with
        | `Key (`Escape, _) -> Navigation ToView
        | `Key (`Enter, _) -> state.confirmMsg
        | _ -> Navigation Nothing
    )

    | Edit state -> (
        match event with
        | `Key (`Arrow `Up, _) -> UI (UpdateField (Name, state.name))
        | `Key (`Arrow `Down, _) -> UI (UpdateField (Necessity, state.necessity))
        | `Key (`Enter, _) ->
            if State.Edit.can_save state
            then Db (EditThing { name = state.name; necessity = state.necessity; someId = state.someId })
            else UI (UpdateField (Necessity, state.necessity))
 
        | `Key (`Uchar uchr, _) ->
            let str = State.Edit.get_curr_field state in
            UI (UpdateField (state.field, Input.add_uchar str uchr))

        | `Key (`ASCII chr, _) ->
            let str = State.Edit.get_curr_field state in
            UI (UpdateField (state.field, Input.add_char str chr))

        | `Key (`Backspace, _) ->
            let str = State.Edit.get_curr_field state in
            UI (UpdateField (state.field, Input.backspace str))

        | `Key (`Escape, _) -> Navigation ToView

        | _ -> Navigation Nothing
    )

let reducer term (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

