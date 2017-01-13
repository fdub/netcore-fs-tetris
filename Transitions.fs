module Fdub.Tetris.Transitions

open Fdub.Tetris.Blocks
open Fdub.Tetris.Model
open Fdub.Tetris.Transformations

let random = System.Random()

let moveRight (state : State) = { state with Point = state.Point @+ toPoint 1 0 }
let moveLeft (state : State) = { state with Point = state.Point @+ toPoint -1 0 }
let moveDown (state : State) = { state with Point = state.Point @+ toPoint 0 1 }

let rotateBlock (state : State) = 
    { state with Block = rotate state.Block
                 Point = state.Point @+ rotateOffset state.Block }

let private reduceRows (grid : Grid) =
    let rows = 
        grid.Rows 
        |> List.filter (fun row -> row.Pixels |> List.forall (fun pixel -> pixel.Value) |> not)
        |> List.map (fun row -> row.Pixels |> List.map (fun pixel -> pixel.Value))
    let emptyRows =
        [0..grid.Size.Height - List.length rows - 1]
        |> List.map (fun row -> [0..grid.Size.Width - 1] |> List.map (fun _ -> false))
    emptyRows@rows |> createGrid

let newBlock (state : State) =
    let grid = match state.Block.Grid.Size.Width with | 0 -> state.Grid | _ -> combine state |> reduceRows
    let block = blocks.[random.Next(List.length blocks)]
    { Grid = grid
      Block = block
      Point = centerOffset grid block }