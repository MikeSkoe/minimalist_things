open Notty

module type IMG = sig
    type props
    val draw : props -> I.t
end

