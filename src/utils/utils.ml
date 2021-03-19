module type FUNCTOR = sig
    type 'a t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
end

module type MONAD = sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionMonad : MONAD with type 'a t := 'a option = struct
    let return x = Some x

    let (>>=) m f = 
        match m with
        | None -> None
        | Some x -> f x
end

module OptionFunctor : FUNCTOR with type 'a t := 'a option = struct
    let (<$>) f =
        function
        | None -> None
        | Some x -> Some (f x)
end

let (>>) f1 f2 arg = arg |> f1 |> f2
let (<<) f1 f2 arg = arg |> f2 |> f1

let ($) f arg = f arg

let get_input name =
      print_endline name;
      read_line ()
