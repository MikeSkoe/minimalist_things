let db = Db.make ()
let term = Notty_unix.Term.create ()

let rec run (state: State.t) =
      function
      | State.Quit -> ()
      | msg -> 
            let state = State.reducer state msg in
            let state = Db.reducer db state msg in
            let msg = Ui.draw term state in
            run state msg

let _ = run (State.to_view (Db.get_data db)) State.Init

