open Notty
open Notty_unix

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
                | `Key (`ASCII chr, _) -> UI (UpdateQuery (true, query ^ (Char.escaped chr)))
                | _ -> System Nothing
            )
            | (false, _query) -> (
                match event with
                | `Key (`Escape, _) -> System Quit
                | `Key (`Arrow `Up, _)
                | `Key (`ASCII 'k', _) -> UI Up

                | `Key (`Arrow `Down, _)
                | `Key (`ASCII 'j', _) -> UI Down

                | `Key (`ASCII 'a', _) -> Navigation (ToEdit None)

                | `Key (`Enter, _)
                | `Key (`ASCII 'e', _) ->
                    Navigation (ToEdit (State.Thing.get_id state.things state.selected))

                | `Key (`Backspace, _)
                | `Key (`ASCII 'd', _) ->
                    let text = "Are your shure, you want to delete the item?"
                    and id = List.(nth state.things state.selected).id
                    in
                    RequestConfirm (text, Db (DeleteThing id))

                | `Key (`ASCII '/', _) -> UI (UpdateQuery (true, ""))

                | `Key (`ASCII 'q', _)

                | _ -> System Nothing
            )
    )

    | Edit state -> (
        match event with
        | `Key (`ASCII 'q', _) -> System Quit
        | `Key (`Arrow `Up, _) -> UI (UpdateField (Name, state.name))
        | `Key (`Arrow `Down, _) -> UI (UpdateField (Necessity, state.necessity))
        | `Key (`Enter, _) ->
            if state.field = Necessity
            && State.can_save state.field state.name state.necessity
                then Db (EditThing { name = state.name; necessity = state.necessity; someId = state.someId })
                else UI (UpdateField (Necessity, state.necessity))

        | `Key (`ASCII chr, _) ->
            if state.field = Name then UI (UpdateField (Name, (state.name ^ (Char.escaped chr))))
            else UI (UpdateField (Necessity, (state.necessity ^ (Char.escaped chr))))

        | `Key (`Backspace, _) ->
            if state.field = Name
                then UI (UpdateField (
                    Name,
                    try String.sub state.name 0 ((String.length state.name) - 1)
                    with Invalid_argument _ -> ""
                ))
                else UI (UpdateField (
                    Necessity,
                    try String.sub state.necessity 0 ((String.length state.necessity) - 1)
                    with Invalid_argument _ -> ""
                ))

        | `Key (`Escape, _) -> Navigation ToView

        | _ -> System Nothing
    )

    | Confirm state -> (
        match event with
        | `Key (`ASCII 'q', _) -> System Quit
        | `Key (`Escape, _) -> Navigation ToView
        | `Key (`Enter, _) -> state.confirmMsg
        | _ -> System Nothing
    )

let img_of_things (things: State.Thing.t list) (selected: int) =
    things
    |> List.mapi (State.Thing.string_of_t selected)
    |> List.map I.(string A.empty)
    |> List.fold_left I.(<->) I.empty

let edit_view field name necessity=
    let info_save = I.(string
        (if State.can_save field name necessity
            then A.empty
            else A.(fg lightblack))
        "[enter]-save "
    ) in
    let info_cancel = I.(string A.empty "[esc]-cancel") in
    let info = I.(info_save <|> info_cancel) in
    let cursor = I.(char A.(bg white) ' ' 1 1) in
    let empty = I.(char A.empty ' ' 1 1) in
    let (name_cursor, necessity_cursor) =
        if field = State.Name then (cursor, empty)
        else (empty, cursor) in
    let name = I.(string A.empty ("Name      : " ^ name)) in
    let necessity = I.(string A.empty ("Necessity : " ^ necessity))
    in I.(
        info
        <-> (name <|> name_cursor)
        <-> (necessity <|> necessity_cursor)
    )

let list_view things selected query =
    let debug = I.(string A.empty ("len: " ^ (string_of_int (List.length things)) ^ " selected" ^ (string_of_int selected))) in
    let query =
        let suffix = I.(char A.empty '/' 1 1) in
        match query with
        | (true, str) -> 
            let cursor = I.(char A.(bg white) ' ' 1 1) in
            let str = I.(string A.empty str) in
            I.(suffix <|> str <|> cursor)
        | (false, str) ->
            let str = I.(string A.empty str) in
            I.(suffix <|> str) in
    let info = I.(string A.empty "[j]-down [k]-up [a]-add item [e]-edit item [d]-delete item")
    and divider = I.(char A.empty '-' 10 1)
    and view = img_of_things things selected
    in I.(debug <-> query <-> info <-> divider <-> view)

let confirm_view text =
    let info = I.(string A.empty text)
    and divider = I.(char A.empty ' ' 1 1)
    and buttons = I.(string A.empty "[enter]-ok [esc]-cancel")
    in I.(info <-> divider <-> buttons)

let draw term state =
    let view =
        match state with
        | State.Edit {field; name; necessity; _} -> edit_view field name necessity
        | State.View {things; selected; query } -> list_view things selected query
        | State.Confirm {text; _} -> confirm_view text
    in Term.image term view

let reducer (term: Term.t) (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

