let db = Db.make "base.db"
let term = Notty_unix.Term.create ()

let rec run ((state, msgs): State.model * State.msg list) =
    let open State in
    match msgs with
    | [`Navigation Navigation.Quit] -> ()
    | msgs -> 
    (state, msgs)
        |> State.reducer db
        |> Ui.reducer term
        |> run

let _ = run State.(View View.initial_model, [`Navigation Navigation.ToView])

