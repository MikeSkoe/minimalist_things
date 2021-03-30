module type PAGE = sig
      type model
      type msg

      val get_message: Notty_unix.Term.t -> model -> msg
      val draw: model -> Notty.I.t
end
