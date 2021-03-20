open Notty
open Notty_unix


module Input = struct
    let add_char str chr = str ^ (Char.escaped chr)

    let add_uchar str uchr =
        let chr_str = 
            let buffer = Buffer.create 1 in
            let _ = Buffer.add_utf_8_uchar buffer uchr in
            Buffer.contents buffer in
        str ^ chr_str

    let backspace str =
        let open Batteries in
        let utf_length = max 0 (BatUTF8.length str) in
        let last_char = BatUTF8.get str (utf_length - 1) in
        let char_size = if BatUChar.is_ascii last_char then 1 else 2 in
        String.sub str 0 (max 0 ((String.length str) - char_size))
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
            in I.(key <|> space <|> action <|> space)
        )
        |> List.fold_left I.(<|>) I.empty
        |> fun info -> I.(info <-> draw_divider)
end

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
                | `Key (`Backspace, _) ->
                    let text = "Are your shure, you want to delete the item?"
                    and id = List.(nth state.things state.selected).id
                    in
                    RequestConfirm (text, Db (DeleteThing id))
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

let img_of_things (things: State.Thing.t list) (selected: int) =
    things
    |> List.mapi (fun index (thing : State.Thing.t) ->
        let selected =
            if index = selected
            then I.(string A.empty "• ")
            else I.(string A.empty "  ")
        and name = I.(string A.(st bold) thing.name)
        and necessity = I.(string A.empty thing.necessity)
        and space = I.(string A.empty ": ")
        in I.(selected <|> name <|> space <|> necessity)
    )
    |> List.fold_left I.(<->) I.empty

let edit_view edit_state =
    let draw_info =
        if State.can_save edit_state
        then Info.(draw [(Enter, "Save"); (Escape, "CANCEL")])
        else Info.(draw [(Escape, "CANCEL")]) in
    let cursor = I.(char A.(bg white) ' ' 1 1) in
    let empty = I.(char A.empty ' ' 1 1) in
    let (name_cursor, necessity_cursor) =
        if edit_state.field = State.Name
        then (cursor, empty)
        else (empty, cursor) in
    let name = I.(string A.empty ("Name      : " ^ edit_state.name)) in
    let necessity = I.(string A.empty ("Necessity : " ^ edit_state.necessity))
    in I.(
        draw_info
        <-> (name <|> name_cursor)
        <-> (necessity <|> necessity_cursor)
    )

let draw_query query = 
    let suffix = I.(char A.empty '/' 1 1) in
    match query with
    | (false, "") -> I.empty
    | (changing, str) -> 
        let cursor = I.(char A.(bg white) ' ' 1 1) in
        let str = I.(string A.empty str) in
        I.(suffix <|> str <|> (if changing = true then cursor else empty))

let list_view (state: State.view_state) =
    let query = draw_query state.query
    and view = img_of_things state.things state.selected
    and draw_info = Info.(draw [(Space, "ADD"); (Enter, "EDIT"); (Backspace, "DELETE"); (Slash, "FIND")]) in
    I.(query <-> draw_info <-> view)

let confirm_view (state: State.confirm_state) =
    let draw_info = Info.(draw [(Enter, "OK"); (Escape, "CANCEL")]) in
    let text = I.(string A.empty state.text) in
    I.(draw_info <-> text)

let draw term state =
    let open State in
    let view =
        match state with
        | Edit state -> edit_view state
        | View state -> list_view state
        | Confirm state -> confirm_view state in
    Term.image term view

let reducer (term: Term.t) (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

