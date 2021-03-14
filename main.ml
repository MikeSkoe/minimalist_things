let db = Db.make ()
let term = Notty_unix.Term.create ()

let rec run (state, msg) =
      match msg with
      | State.Quit -> ()
      | msg -> 
          (state, msg)
          |> State.reducer
          |> Db.reducer db
          |> Ui.reducer term
          |> run

let _ = run (State.initial_state, State.Init)

