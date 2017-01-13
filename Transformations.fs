module Fdub.Tetris.Transformations

open Fdub.Tetris.Model
open Fdub.Tetris.Blocks

type State = 
    { Grid : Grid
      Block : Block
      Point : Point }

let rotate (block : Block) =
    [0..block.Grid.Size.Width - 1] 
    |> List.map (fun x ->
        [0..block.Grid.Size.Height - 1] 
        |> List.map (fun y -> value block.Grid (toPoint (block.Grid.Size.Width - x - 1) y))
    )
    |> createGrid
    |> blockFromGrid

let rotateOffset (block : Block) =
    match block.Grid.Size with
    | { Width = 4; Height = 1 } -> toPoint 1 -1
    | { Width = 1; Height = 4 } -> toPoint -1 1
    | _ -> toPoint 0 0

let private isValidBlockPosition (state : State) (point : Point) =
    let block = state.Block
    let offset = state.Point
    (point.X - offset.X) >= 0 && (point.X - offset.X) < block.Grid.Size.Width &&
    (point.Y - offset.Y) >= 0 && (point.Y - offset.Y) < block.Grid.Size.Height

let combine (state : State) =
    [0..state.Grid.Size.Height - 1]
    |> List.map (fun y ->
        [0..state.Grid.Size.Width - 1]
        |> List.map (fun x ->
            let point = toPoint x y
            if isValidBlockPosition state point then
                value state.Grid point || value state.Block.Grid (point @- state.Point)
            else
                value state.Grid point
        )
    )
    |> createGrid

let canCombine (state : State) =
    let offsetValid =  
        (state.Point @+ state.Block.Start).X >= 0 && 
        (state.Point @+ state.Block.Start).Y >= 0 &&
        (state.Point @+ state.Block.End).X <= state.Grid.Size.Width && 
        (state.Point @+ state.Block.End).Y <= state.Grid.Size.Height

    let combinationValid = 
        [0..state.Grid.Size.Height - 1]
        |> List.forall (fun y ->
            [0..state.Grid.Size.Width - 1]
            |> List.forall (fun x ->
                let point = toPoint x y
                if isValidBlockPosition state point then
                    not (value state.Grid point && value state.Block.Grid (point @- state.Point))
                else
                    true
            )
        )
    
    offsetValid && combinationValid

let centerOffset (grid : Grid) (block : Block) = toPoint ((grid.Size.Width - block.Grid.Size.Width) / 2) 0
