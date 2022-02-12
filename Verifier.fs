module Verifier

// I like the Ruby-esque way of naming predicates ending with "?"
// In F#, though, you can't name variable ending with "?" by default, but you can use the double-backtick notation, ``valName``
// to define any arbitrary string as the variable name
let ``isValidEntity?`` entity =
    let validSet = set [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    let entitySet = set entity

    // A valid entity **must** produce an empty set when "subtracted" from the valid set
    Set.difference validSet entitySet = Set.empty

let parseInput (input: string) =
    input.ToCharArray()
    |> Array.chunkBySize 9
    |> Array.map (fun arr -> Array.map (fun elem -> int elem - int '0') arr) // Convert the input string to array-of-arrays, each 9 elements longs, all `int`s. The `- int '0'` is to get the true value of the **char** and **not** its ASCII value!
    |> array2D

let getRows (parsedInput: int [,]) =
    [| for i in 0 .. (parsedInput.GetLength(0) - 1) -> parsedInput[i, *] |]

let getColumns (parsedInput: int[,]) =
    [| for i in 0 .. (parsedInput.GetLength(1) - 1) -> parsedInput[*, i] |]

// Calculating boxes validity is redundant: if a row/column is invalid, by definition it invalidates the box in which contains the wrong number!
// let getBoxes solutionRows =
//     let chunkSize = 3 // No "magic numbers" here. 3 due to the boxes each being 3 X 3 in size
//     let interim = solutionRows |> Array.collect (Array.chunkBySize chunkSize)
//     seq { for i in 0 .. 2 ->
//             seq { for j in 0 .. ((Array.length interim / chunkSize) - 1) ->
//                     interim[i + (chunkSize * j)]}}
//     |> Seq.map Seq.toArray // Convert the inner seqs to arrays
//     |> Seq.collect id // Collect all inner arrays to one external seq-of-arrays
//     |> Seq.toArray // Convert to the final value of array-of-arrays

let validateAllEntities (entities: int array array) =
    Array.map ``isValidEntity?`` entities
    |> Array.contains false
    |> function
        | false -> false
        | true -> true
