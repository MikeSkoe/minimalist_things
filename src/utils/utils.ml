let (>>) f1 f2 arg = arg |> f1 |> f2
let (<<) f1 f2 arg = arg |> f2 |> f1
