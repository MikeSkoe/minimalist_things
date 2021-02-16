(* let () = Ui.run () *)
let db = Db.Index.make ()
let term = Notty_unix.Term.create ()

let get_initial_state () : Entities.Index.t = ({
      things = [];
      selected = 0;
      view_type = Entities.Index.View;
})

let rec reducer (state: Entities.Index.t) =
      function
      | Ui.Quit -> state
      | Ui.Init -> {
            things = Db.Thing_db.get_data db;
            selected = 0;
            view_type = Entities.Index.View;
      }
      | Ui.Up -> {
            state with
            selected = max 0 (state.selected - 1)
      }
      | Ui.Down -> {
            state with
            selected = min (List.length state.things - 1) (state.selected + 1)
      }
      | Ui.AddView str -> {
            state with
            view_type = Entities.Index.Add str
      }
      | Ui.AddThing str ->
            let strings = String.split_on_char ':' str in
            let title = List.nth strings 0
            and necessity = List.nth strings 1
            in
            if Db.Thing_db.add db title necessity = true
                  then reducer state Ui.Init
                  else state
      | Ui.DeleteThing id ->
            if Db.Thing_db.delete db id = true
                  then reducer state Ui.Init
                  else state

let rec run (state: Entities.Index.t) =
      function
      | Ui.Quit -> Ui.Quit
      | msg -> 
            let state = reducer state msg in
            run state (Ui.draw term state)

let _ = run (get_initial_state ()) Ui.Init

(*
let action =
      try Sys.argv.(1)
      with Invalid_argument _ -> ""

let result_string = match Sys.argv with
      | [|_; "show_things"|] -> Controller.show_things ()
      | [|_; "add_thing"|] -> Controller.add_thing ()
      | [|_; "delete_thing"|] -> Controller.delete_thing ()
      | [|_; "update_thing"|] -> Controller.update_thing ()
      | [|_; "show_groups"|] -> Controller.show_groups ()
      | [|_; "add_group"|] -> Controller.add_group ()
      | [|_; "delete_group"|] -> Controller.delete_group ()
      | _ -> "Available commands:\n"
            ^ "show_things\n"
            ^ "add_thing\n"
            ^ "delete_thing\n"
            ^ "update_thing\n"
            ^ "show_groups\n"
            ^ "add_group\n"
            ^ "delete_group\n"

let _ = print_endline result_string
*)

