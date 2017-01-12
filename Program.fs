module Fdub.Tetris.Program

open System
open Fdub.Tetris.Model
open Fdub.Tetris.Init
open Fdub.Tetris.Blocks
open Fdub.Tetris.Display
open Fdub.Tetris.Transformations   

let private random = Random()
let private newBlock (grid : Grid) =
    let block = blocks.[random.Next (List.length blocks)]
    { Grid = grid
      Block = block 
      X = centerOffsetX grid block 
      Y = 0 }

let rec private loop (state : Combination) =
    show (combine state)
    let key = Console.ReadKey(false)
    match key.Key with
    | ConsoleKey.Escape -> 
        ()
    | ConsoleKey.RightArrow when canCombine { state with X = (state.X + 1) } -> 
        loop { state with X = (state.X + 1) }
    | ConsoleKey.LeftArrow when canCombine { state with X = (state.X - 1) } -> 
        loop { state with X = (state.X - 1) }
    | ConsoleKey.UpArrow when canCombine { state with Y = (state.Y - 1) } -> 
        loop { state with Y = (state.Y - 1) }
    | ConsoleKey.DownArrow when canCombine { state with Y = (state.Y + 1) } -> 
        loop { state with Y = (state.Y + 1) }
    | ConsoleKey.Spacebar when canCombine { state with Block = rotate state.Block
                                                       X = state.X + rotateOffsX state.Block
                                                       Y = state.Y + rotateOffsY state.Block } -> 
        loop { state with Block = rotate state.Block
                          X = state.X + rotateOffsX state.Block
                          Y = state.Y + rotateOffsY state.Block }
    | ConsoleKey.Enter ->
        let newState = newBlock (combine state)
        if not <| canCombine newState then
            printfn "Game over" 
        else loop newState
    | _ -> loop state

[<EntryPoint>]
let main argv = 
    let grid = initGrid 10 20
    loop <| newBlock grid
    0 // return an integer exit code
