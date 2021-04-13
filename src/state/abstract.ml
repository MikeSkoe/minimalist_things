module State_Thing = Thing

module type DB = sig
      type t
      val db: t

      module Thing: sig
            val get: t -> string option -> State_Thing.t list
            val get_one: t -> int -> State_Thing.t option
            val delete: t -> int -> bool
            val add: t -> string -> string -> bool
      end
end
