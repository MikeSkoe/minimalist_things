open Notty

type props = {
      msg: string option;
}

module Img : Image.IMG with type props := props = struct
      let draw {msg} =
            match msg with
            | Some msg -> I.(string A.empty msg |> vpad 0 1)
            | None -> I.empty
end
