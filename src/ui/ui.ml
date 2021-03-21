open Notty
open Notty_unix

module Input = struct
    let add_char str chr = str ^ (Char.escaped chr)

    let add_uchar str uchr =
        let chr_str = 
            let buffer = Buffer.create 1 in
            let _ = Buffer.add_utf_8_uchar buffer uchr in
            Buffer.contents buffer
        in
        str ^ chr_str

    let backspace str =
        let open Batteries in
        let utf_length = max 0 (BatUTF8.length str) in
        let last_char = BatUTF8.get str (utf_length - 1) in
        let char_size = if BatUChar.is_ascii last_char then 1 else 2
        in
        String.sub str 0 (max 0 ((String.length str) - char_size))

    let draw label value active =
        let label = I.string A.(st bold) label
        and divider = I.(string A.empty " : ")
        and cursor =
            if active
            then I.(char A.(bg white) ' ' 1 1)
            else I.empty
        and value = I.(string A.empty value)
        in
        I.(label <|> divider <|> value <|> cursor)
end

module Info = struct
    type key =
        | Space
        | Enter
        | Backspace
        | Slash
        | Escape

    let string_of_key =
        function
        | Space -> "⎵"
        | Enter -> "⏎"
        | Backspace -> "⌫"
        | Slash -> "/"
        | Escape -> "⎋"

    type prop = key * string

    let draw_divider = I.(char A.empty ' ' 1 1)

    let draw props =
        props
        |> List.map (fun (key, action) ->
            let key = key |> string_of_key |> I.(string A.(st bold))
            and action = I.(string A.(fg lightblack) action)
            and space = I.(string A.empty " ")
            in
            I.(key <|> space <|> action <|> space)
        )
        |> List.fold_left I.(<|>) I.empty
        |> fun info -> I.(info <-> draw_divider)
end
module Thing = struct
    let draw (thing: State.Thing.t) selected =
        let suffix =
            if selected
            then I.(string A.empty "• ")
            else I.(string A.empty "  ") in
        let name = I.(string A.(st bold) thing.name) in
        let necessity = I.(string A.empty thing.necessity) in
        let divider = I.(string A.empty " : ")
        in
        I.(suffix <|> name <|> divider <|> necessity)
end

module Query = struct
    let draw =
        function
        | (false, "") -> I.empty
        | (changing, str) -> Input.(draw "FIND" str changing)
end

let list_view (state: State.view_state) =
    let active_actions = [(Info.Space, "ADD"); (Info.Enter, "EDIT"); (Info.Backspace, "DELETE"); (Info.Slash, "FIND")]
    in
    I.(
        Query.(draw state.query)
        <-> Info.(draw active_actions)
        <-> (
            state.things
            |> List.mapi (fun index thing -> Thing.draw thing (index = state.selected))
            |> List.fold_left I.(<->) I.empty
        )
    )

let edit_view edit_state =
    let active_actions = 
        if State.can_save edit_state
        then [(Info.Enter, "Save"); (Info.Escape, "CANCEL")]
        else [(Info.Escape, "CANCEL")] in
    let is_name_active = (edit_state.field = State.Name) in
    let is_necessity_active = (edit_state.field = State.Necessity)
    in
    I.(
        Info.(draw active_actions)
        <-> Input.(draw "Name     " edit_state.name is_name_active)
        <-> Input.(draw "Necessity" edit_state.necessity is_necessity_active)
    )

let confirm_view (state: State.confirm_state) =
    let active_actions = [(Info.Enter, "OK"); (Info.Escape, "CANCEL")]
    in
    I.(
        Info.(draw active_actions)
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
                | _ -> System Nothing
            )
            | (false, query) -> (
                match event with
                | `Key (`Escape, _) ->
                    if query = ""
                    then System Quit
                    else UI (UpdateQuery (false, ""))

                | `Key (`Arrow `Up, _) -> UI Up
                | `Key (`Arrow `Down, _) -> UI Down
                | `Key (`ASCII ' ', _) -> Navigation (ToEdit None)
                | `Key (`Enter, _) -> Navigation (ToEdit (Thing.get_id state.things state.selected))
                | `Key (`Backspace, _) -> (
                    try RequestConfirm (
                        "Are your shure, you want to delete the item?",
                        Db (DeleteThing List.(nth state.things state.selected).id)
                    )
                    with Failure _ -> System Nothing
                )
                | `Key (`ASCII '/', _) -> UI (UpdateQuery (true, ""))
                | `Key (`ASCII 'q', _)
                | _ -> System Nothing
            )
    )

    | Confirm state -> (
        match event with
        | `Key (`Escape, _) -> Navigation ToView
        | `Key (`Enter, _) -> state.confirmMsg
        | _ -> System Nothing
    )

    | Edit state -> (
        match event with
        | `Key (`Arrow `Up, _) -> UI (UpdateField (Name, state.name))
        | `Key (`Arrow `Down, _) -> UI (UpdateField (Necessity, state.necessity))
        | `Key (`Enter, _) ->
            if State.can_save state
            then Db (EditThing { name = state.name; necessity = state.necessity; someId = state.someId })
            else UI (UpdateField (Necessity, state.necessity))
 
        | `Key (`Uchar uchr, _) ->
            let str = State.get_curr_field state in
            UI (UpdateField (state.field, Input.add_uchar str uchr))

        | `Key (`ASCII chr, _) ->
            let str = State.get_curr_field state in
            UI (UpdateField (state.field, Input.add_char str chr))

        | `Key (`Backspace, _) ->
            let str = State.get_curr_field state in
            UI (UpdateField (state.field, Input.backspace str))

        | `Key (`Escape, _) -> Navigation ToView

        | _ -> System Nothing
    )

let reducer (term: Term.t) (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

