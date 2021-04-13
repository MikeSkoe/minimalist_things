module MyState = State.Make(Db)

let rec run (state, msgs) =
    let open State in
    match msgs with
    | [`Navigation Navigation.Quit] -> ()
    | msgs -> 
    (state, msgs)
        |> MyState.reducer
        |> Ui.reducer
        |> run

let _ = run MyState.(View View_Page.initial_model, [`Navigation State.Navigation.ToView])

