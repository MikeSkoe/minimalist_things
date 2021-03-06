open Notty

let add_char str chr = str ^ (Char.escaped chr)

let add_uchar str uchr =
      let chr_str = 
            let buffer = Buffer.create 1 in
            let _ = Buffer.add_utf_8_uchar buffer uchr in
            Buffer.contents buffer in
      str ^ chr_str

let backspace str =
      let open Batteries in
      let utf_next_length = max 0 ((BatUTF8.length str) - 1) in
      let last_char = BatUTF8.get str utf_next_length in
      let char_size =
            if BatUChar.is_ascii last_char
            then 1
            else 2 in
      String.sub str 0 (max 0 ((String.length str) - char_size))

type props = {
      label: string;
      value: string;
      active: bool;
}

module Img : Image.IMG with type props := props = struct
      let draw { label; value; active } =
            let label = I.string A.(st bold) label in
            let divider = I.(string A.empty " : ") in
            let cursor =
                  if active
                  then I.(char A.(bg white) ' ' 1 1)
                  else I.empty in
            let value = I.(string A.empty value) in
            I.(label <|> divider <|> value <|> cursor)
end

