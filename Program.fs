open Verifier

[<EntryPointAttribute>]
let main _ =
    let input =
        "123456789123456789123456789123456789123456789123456789123456789123456789123456789"

    let rows = input |> (parseInput >> getRows) // No good reason to favor composition over piping. Simply adhering to the thumb rule "pipes are for data passing, composition is for functions"
    let cols = input |> (parseInput >> getColumns)

    let ``rowsValid?`` = rows |> validateAllEntities
    let ``colsValid?`` = cols |> validateAllEntities

    match (``rowsValid?`` && ``colsValid?``) with
    | false -> printfn "Solution is invalid"
    | true -> printfn "Solution is valid"

    0
