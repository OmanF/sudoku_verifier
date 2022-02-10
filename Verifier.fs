module Verifier

type EntityType =
    | Row
    | Column
    | Box

type EntityValidity =
    | Valid
    | IncorrectRow of string
    | IncorrectColumn of string
    | IncorrectBox of string

let isValidEntity entity =
    // A valid sudoku entity (i.e., row, column or 3x3 cube) **must** contain all numbers 1 through 9, inclusive
    let validSet = set [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    let entitySet = set entity

    // A valid entity **must** produce an empty set when "subtracted" from the valid set
    Set.difference validSet entitySet = Set.empty

let parseInput (input: string) =
    input.ToCharArray()
    |> Array.chunkBySize 9
    |> Array.map (fun arr -> Array.map (fun elem -> int elem - int '0') arr) // Convert the input string to array-of-arrays, each 9 elements longs, all `int`s. The `- int '0'` is to get the true value of the **char** and **not** its ASCII value!
    |> array2D

let solutionRows (parsedInput: int [,]) =
    [| for i in 0 .. (parsedInput.GetLength(0) - 1) -> parsedInput[i, *] |]

let solutionColumns (parsedInput: int[,]) =
    [| for i in 0 .. (parsedInput.GetLength(1) - 1) -> parsedInput[*, i] |]

let solutionBoxes solutionRows =
    let interim = solutionRows |> Array.collect (Array.chunkBySize 3)
    // TODO: Implment

let validateAllEntities (entities: int array array) (entityType: EntityType) =
    Array.map isValidEntity entities
    |> Array.contains false
    |> function
        | false -> Ok Valid
        | true ->
            match entityType with
            | Row -> Error(IncorrectRow "At least one row is incorrect")
            | Column -> Error(IncorrectColumn "At least one column is incorrect")
            | Box -> Error(IncorrectBox "At least one box is incorrect")
