open Notty

type props = {
    query: bool * string;
}

module Img: Image.IMG with type props := props = struct
    let draw {query} =
        match query with
        | (false, "") -> I.empty
        | (active, value) -> Input.Img.(draw { label = "FIND"; value; active; })
end

