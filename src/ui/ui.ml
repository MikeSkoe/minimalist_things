open Notty
open Notty_unix
open Utils

type msg =
      | Init
      | Up
      | Down
      | Quit
      | AddView of string
      | AddThing of string
      | DeleteThing of int

let getEvent term (state: Entities.Index.t) =
      match (state.view_type, Term.event term) with
      | (Entities.Index.View, `Key (`ASCII 'k', _)) ->
            Up
      | (Entities.Index.View, `Key (`ASCII 'j', _)) ->
            Down
      | (Entities.Index.View, `Key (`ASCII 'a', _)) ->
            AddView ""
      | (Entities.Index.View, `Key (`ASCII 'd', _)) ->
            DeleteThing (List.nth state.things state.selected).id

      | (Entities.Index.Add str, `Key (`ASCII chr, _)) ->
            AddView (str ^ (Char.escaped chr))
      | (Entities.Index.Add str, `Key (`Enter, _)) ->
            AddThing str
      | (Entities.Index.Add str, `Key (`Backspace, _)) ->
            AddView (String.sub str 0 ((String.length str) - 1))

      | _ -> Quit

let info () =
      I.(string A.empty "[j]-down [k]-up [a]-add item [d]-delete item")

let img_of_things (selected:int) =
      List.mapi (Entities.Thing.string_of_t selected)
      >> List.map I.(string A.empty)
      >> List.fold_left I.(<->) I.empty

let adding_view str =
      let tip = I.(string A.empty "Type name and description: ")
      and input = I.(string A.empty str)
      and cursor = I.(char A.(bg white) ' ' 1 1)
      in
      I.(tip <-> (input <|> cursor))

let draw (term: Term.t) (state: Entities.Index.t): msg =
      let info = info ()
      and divider = I.(char A.empty '-' 10 1)
      and view = match state.view_type with
            | Entities.Index.View ->
                  img_of_things state.selected state.things
            | Entities.Index.Add str ->
                  adding_view str
      in
      Term.image term I.(info <-> divider <-> view);

      getEvent term state
