module Fdub.Tetris.Program

open System
open Fdub.Tetris
open Fdub.Tetris.Model
open Fdub.Tetris.Blocks
open Fdub.Tetris.Display
open Fdub.Tetris.Transformations   
open Fdub.Tetris.Transitions

let timeout = 750.

let rec private loop (state : State) =
    show (combine state)

    match Input.readKey() with
    | None when canCombine (moveDown state) -> 
        loop (moveDown state) 
    | None -> 
        let newState = newBlock state
        if not <| canCombine newState then
            printfn "Game over" 
        else loop newState
    | Some key ->
        match key with
        | ConsoleKey.Escape -> ()
        | ConsoleKey.RightArrow when canCombine (moveRight state) -> loop (moveRight state)
        | ConsoleKey.LeftArrow when canCombine (moveLeft state) -> loop (moveLeft state)
        | ConsoleKey.UpArrow  when canCombine (rotateBlock state) -> loop (rotateBlock state)
        | ConsoleKey.DownArrow ->
            let rec moveAllDown = function
                | s when canCombine (moveDown s) -> moveAllDown (moveDown s)
                | s -> s
            Input.tickTimer timeout
            loop (moveAllDown state)
        | _ -> loop state

[<EntryPoint>]
let main argv = 
    Input.init timeout

    let initialState = 
        { Grid = initGrid 10 20
          Block = empty
          Point = toPoint 0 0 }
    loop (newBlock initialState)
    
    Input.stop()
    0 // return an integer exit code
