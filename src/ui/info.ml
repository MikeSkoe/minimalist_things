open Notty

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

let draw_divider = I.(char A.empty ' ' 1 1)

type action = key * string

type props = {
      actions: action list;
}

module Img : Image.IMG with type props := props = struct
      let draw {actions} =
            actions
            |> List.map (fun (key, action) ->
                  let key = key |> string_of_key |> I.(string A.(st bold))
                  and action = I.(string A.(fg lightblack) action)
                  and space = I.(string A.empty " ") in
                  I.(key <|> space <|> action <|> space)
            )
            |> List.fold_left I.(<|>) I.empty
            |> fun info -> I.(info <-> draw_divider)
end

