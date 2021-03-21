let db = Db.make "base.db"
let term = Notty_unix.Term.create ()

let rec run (state, msg) =
    let open State in
    match msg with
    | System Quit -> ()
    | msg -> 
    (state, msg)
        |> State.reducer
        |> Db.reducer db
        |> Ui.reducer term
        |> run

let _ = run State.(View View.(make [] (false, "")), Navigation ToView)

