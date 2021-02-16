open Notty

type word = {
  id: int;
  title: string;
}

type t = {
  selected: int;
  words: word list;
}

type shift =
  | Up
  | Down

let make strs : t = ({
  selected = 0;
  words = List.mapi (fun id title -> {id; title;}) strs;
})

let is_selected (selected, _word_list) (id, _str) =
  if id = selected then true
  else false

let move {selected; words;} =
  let til_zero = 0 |> max
  and til_len = (List.length words) - 1 |> min
  in

  function
    | Up -> {
      selected = selected - 1 |> til_zero;
      words;
    }
    | Down -> {
      selected = selected + 1 |> til_len;
      words;
    }

let img_of_words t =
  let curr = I.string A.empty ". "
  and not_curr = I.string A.empty "  "
  in 

  t.words
  |> List.map
    (fun {id; title} ->
      I.(<|>)
        (if id = t.selected then curr else not_curr)
        (I.string A.empty title)
    )
  |> List.fold_left
    (fun img str -> I.(<->) img str)
    (I.string A.empty "----");

