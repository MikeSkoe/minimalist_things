open Notty
open Notty_unix

let getEvent term state =
    let open State in
    let event = Term.event term
    in match state with
    | View state -> (
        match event with
        | `Key (`Escape, _) -> Quit
        | `Key (`Arrow `Up, _)
        | `Key (`ASCII 'k', _) -> Up

        | `Key (`Arrow `Down, _)
        | `Key (`ASCII 'j', _) -> Down

        | `Key (`ASCII 'a', _) -> ToEdit None

        | `Key (`Enter, _)
        | `Key (`ASCII 'e', _) ->
            ToEdit (State.Thing.get_id state.things state.selected)

        | `Key (`Backspace, _)
        | `Key (`ASCII 'd', _) ->
            let text = "Are your shure, you want to delete the item?"
            and id = List.(nth state.things state.selected).id
            in
            RequestConfirm (text, DeleteThing id)

        | `Key (`ASCII 'q', _)

        | _ -> Nothing
    )

    | Edit state -> (
        match event with
        | `Key (`ASCII 'q', _) -> Quit
        | `Key (`Arrow `Up, _) -> UpdateField (Title, state.title)
        | `Key (`Arrow `Down, _) -> UpdateField (Description, state.description)
        | `Key (`Enter, _) ->
            if state.field = Description
            && State.can_save state.field state.title state.description
                then EditThing
                else UpdateField (Description, state.description)

        | `Key (`ASCII chr, _) ->
            if state.field = Title then UpdateField (Title, (state.title ^ (Char.escaped chr)))
            else UpdateField (Description, (state.description ^ (Char.escaped chr)))

        | `Key (`Backspace, _) ->
            if state.field = Title
                then UpdateField (
                    Title,
                    try String.sub state.title 0 ((String.length state.title) - 1)
                    with Invalid_argument _ -> ""
                )
                else UpdateField (
                    Description,
                    try String.sub state.description 0 ((String.length state.description) - 1)
                    with Invalid_argument _ -> ""
                )

        | `Key (`Escape, _) -> Init

        | _ -> Nothing
    )

    | Confirm state -> (
        match event with
        | `Key (`ASCII 'q', _) -> Quit
        | `Key (`Escape, _) -> Init
        | `Key (`Enter, _) -> state.confirmMsg
        | _ -> Nothing
    )

let img_of_things (things: State.Thing.t list) (selected: int) =
    things
    |> List.mapi (State.Thing.string_of_t selected)
    |> List.map I.(string A.empty)
    |> List.fold_left I.(<->) I.empty

let edit_view field title description=
    let info_save = I.(string
        (if State.can_save field title description
            then A.empty
            else A.(fg lightblack))
        "[enter]-save "
    ) in
    let info_cancel = I.(string A.empty "[esc]-cancel") in
    let info = I.(info_save <|> info_cancel) in
    let cursor = I.(char A.(bg white) ' ' 1 1) in
    let empty = I.(char A.empty ' ' 1 1) in
    let (name_cursor, description_cursor) =
        if field = State.Title then (cursor, empty)
        else (empty, cursor) in
    let name = I.(string A.empty ("Name        : " ^ title)) in
    let description = I.(string A.empty ("Description : " ^ description))
    in I.(
        info
        <-> (name <|> name_cursor)
        <-> (description <|> description_cursor)
    )

let list_view things selected =
    let info = I.(string A.empty "[j]-down [k]-up [a]-add item [e]-edit item [d]-delete item")
    and divider = I.(char A.empty '-' 10 1)
    and view = img_of_things things selected
    in I.(info <-> divider <-> view)

let confirm_view text =
    let info = I.(string A.empty text)
    and divider = I.(char A.empty ' ' 1 1)
    and buttons = I.(string A.empty "[enter]-ok [esc]-cancel")
    in I.(info <-> divider <-> buttons)

let draw term state =
    let view =
        match state with
        | State.Edit {field; title; description; _} -> edit_view field title description
        | State.View {things; selected} -> list_view things selected
        | State.Confirm {text; _} -> confirm_view text
    in Term.image term view

let reducer (term: Term.t) (state, _msg) =
    let _ = draw term state
    and msg = getEvent term state
    in (state, msg)

