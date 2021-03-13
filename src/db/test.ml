module type Monad = sig
      type 'a t
      val return : 'a -> 'a t
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad with type 'a t = 'a option = struct
      type 'a t = 'a option
    
      let return x = Some x
    
      let (>>=) m f = 
            match m with
            | None -> None
            | Some x -> f x
end

open Maybe

let div a b =
      if b = 0 then None
      else return (a / b)

let upgrade_binary op x y =
      x >>= fun a ->
      y >>= fun b ->
      op a b
    
let return_binary op x y = return (op x y)
    
let ( + ) = upgrade_binary (return_binary ( + ))
let ( - ) = upgrade_binary (return_binary ( - ))
let ( * ) = upgrade_binary (return_binary ( * ))
let ( / ) = upgrade_binary div
