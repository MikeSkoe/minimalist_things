open Notty

type props = {
    thing: State.Thing.t;
    selected: bool;
}

module Img : Image.IMG with type props := props = struct
    let draw {thing; selected} =
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

