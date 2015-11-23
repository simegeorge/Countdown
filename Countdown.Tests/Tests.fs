module CountdownTests

open Xunit
open CountdownProblem

[<Fact>]
let ``Optimal method should find a valid solution`` () =

    // Use the most optimal solution finder
    let solutions = solutions'' [1; 3; 7; 10; 25; 50] 765

    // One solution should be (25 - 10) * (1 + 50)
    let oneSolution = App (Mul, App (Sub,(Val 25),(Val 10)), App (Add,(Val 1),(Val 50)) )

    solutions 
    |> List.contains oneSolution 
    |> Assert.True
