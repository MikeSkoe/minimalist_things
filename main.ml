module MyState = State.Make(Db)

let rec loop (state, msgs) =
    let open State in
    match msgs with
    | [`Navigation Navigation.Quit] -> ()
    | msgs -> 
    (state, msgs)
        |> MyState.reducer
        |> Ui.reducer
        |> loop

let _ = loop MyState.(View View_Page.initial_model, [`Navigation State.Navigation.ToView])

