module Fdub.Tetris.Program

open System
open Fdub.Tetris.Model
open Fdub.Tetris.Blocks
open Fdub.Tetris.Display
open Fdub.Tetris.Transformations   
open Fdub.Tetris.Transitions

let rec private loop (state : State) =
    show (combine state)
    let key = Console.ReadKey(false)
    match key.Key with
    | ConsoleKey.Escape -> 
        ()
    | ConsoleKey.RightArrow when canCombine (moveRight state) -> loop (moveRight state)
    | ConsoleKey.LeftArrow when canCombine (moveLeft state) -> loop (moveLeft state)
    | ConsoleKey.UpArrow when canCombine (moveUp state) -> loop (moveUp state)
    | ConsoleKey.DownArrow when canCombine (moveDown state) -> loop (moveDown state)
    | ConsoleKey.Spacebar when canCombine (rotateBlock state) -> loop (rotateBlock state)
    | ConsoleKey.Enter ->
        let newState = newBlock state
        if not <| canCombine newState then
            printfn "Game over" 
        else loop newState
    | _ -> loop state

[<EntryPoint>]
let main argv = 
    let initialState = 
        { Grid = initGrid 10 20
          Block = empty
          Point = toPoint 0 0 }
    loop (newBlock initialState)
    0 // return an integer exit code
