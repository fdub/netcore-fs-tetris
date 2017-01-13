module Fdub.Tetris.Blocks

open Fdub.Tetris.Model

type Block = 
    { Grid : Grid
      Start : Point
      End : Point }

let private blockStart (grid : Grid) =
    let startX = 
        [0..grid.Size.Width - 1]
        |> List.find (fun x ->
            [0..grid.Size.Height - 1]
            |> List.exists (fun y -> value grid (toPoint x y))
        )
    let startY =
        [0..grid.Size.Height - 1]
        |> List.find (fun y ->
            [0..grid.Size.Width - 1]
            |> List.exists (fun x -> value grid (toPoint x y))
        ) 
    { X = startX; Y = startY }
let blockEnd (grid : Grid) =
    let endX =
        [0..grid.Size.Width - 1]
        |> List.rev
        |> List.find (fun x ->
            [0..grid.Size.Height - 1]
            |> List.exists (fun y -> value grid (toPoint x y))
        ) 
        |> (+) 1
    let endY =
        [0..grid.Size.Height - 1]
        |> List.rev
        |> List.find (fun y ->
            [0..grid.Size.Width - 1]
            |> List.exists (fun x -> value grid (toPoint x y))
        ) 
        |> (+) 1
    { X = endX; Y = endY }

let blockFromGrid grid = { Grid = grid; Start = blockStart grid; End = blockEnd grid }

let private createBlock data =
    data  
        |> List.map (List.map (fun i -> i > 0))
        |> createGrid
        |> blockFromGrid

let empty = { Grid = { Rows = List.empty; Size = toSize 0 0 }; Start = toPoint 0 0; End = toPoint 0 0 }

let blocks =
    [ [ [1; 1]
        [1; 1] ]
      [ [0; 1]
        [1; 1]
        [1; 0] ]
      [ [1; 0]
        [1; 1]
        [0; 1] ]
      [ [1]
        [1]
        [1]
        [1] ]
      [ [0; 1; 0]
        [0; 1; 0]
        [1; 1; 0] ]
      [ [0; 1; 0] 
        [0; 1; 0]
        [0; 1; 1] ] 
      [ [0; 1; 0]
        [1; 1; 1]
        [0; 0; 0] ] ]
    |> List.map createBlock
