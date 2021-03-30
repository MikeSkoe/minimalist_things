open Notty

type props = {
      name: string;
      necessity: string;
      selected: bool;
}

module Img : Image.IMG with type props := props = struct
      let draw {name; necessity; selected} =
      let suffix =
            if selected
            then I.(string A.empty "• ")
            else I.(string A.empty "  ") in
      let name = I.(string A.(st bold) name) in
      let necessity = I.(string A.empty necessity) in
      let divider = I.(string A.empty " : ")
      in
      I.(suffix <|> name <|> divider <|> necessity)
end

