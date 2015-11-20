
[<EntryPoint>]
let main argv = 

    let solutions = CountdownProblem.solutions'' [1; 3; 7; 10; 25; 50] 765

    solutions
    |> Seq.iter (printfn "%s" << CountdownProblem.showExpr)

    0
