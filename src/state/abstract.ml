module type DB = sig
      type t
      val db: t

      module Thing: sig
            type thing_row = {
                  id: int;
                  name: string;
                  necessity: string;
            }

            val get_data: t -> string option -> thing_row list
            val get_thing: t -> int -> thing_row option
            val delete_thing: t -> int -> bool
            val add_thing: t -> string -> string -> bool
      end
end
