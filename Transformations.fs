module Fdub.Tetris.Transformations

open Fdub.Tetris.Model

type Combination = 
    { Grid : Grid
      Block : Grid
      X : int
      Y : int }
let private gridSize (grid : Grid) = { Width = List.length grid.Rows.[0].Pixels; Height = List.length grid.Rows }
let private pixelVal (grid : Grid) x y = grid.Rows.[y].Pixels.[x].Pixel
let rotate (block : Grid) =
    let size = gridSize block
    [0..size.Width - 1] 
    |> List.map (fun x ->
        [0..size.Height - 1] 
        |> List.map (fun y -> { Pixel = pixelVal block (size.Width - x - 1) y } )
        |> fun pixels -> { Pixels = pixels } 
    )
    |> fun rows -> { Rows = rows }

let rotateOffsX (block : Grid) =
    match gridSize block with
    | { Width = 4; Height = 1 } -> 1
    | { Width = 1; Height = 4 } -> -1
    | _ -> 0

let rotateOffsY (block : Grid) =
    match gridSize block with
    | { Width = 4; Height = 1 } -> -1
    | { Width = 1; Height = 4 } -> 1
    | _ -> 0

let private blockStartX (block : Grid) =
    let size = gridSize block
    [0..size.Width - 1]
    |> List.filter (fun x ->
        [0..size.Height - 1]
        |> List.exists (fun y -> pixelVal block x y)
    )
    |> List.min
let private blockEndX (block : Grid) =
    let size = gridSize block
    [0..size.Width - 1]
    |> List.filter (fun x ->
        [0..size.Height - 1]
        |> List.exists (fun y -> pixelVal block x y)
    ) 
    |> List.max
    |> (+) 1
let private blockStartY (block : Grid) =
    let size = gridSize block
    [0..size.Height - 1]
    |> List.filter (fun y ->
        [0..size.Width - 1]
        |> List.exists (fun x -> pixelVal block x y)
    ) 
    |> List.min
let private blockEndY (block : Grid) =
    let size = gridSize block
    [0..size.Height - 1]
    |> List.filter (fun y ->
        [0..size.Width - 1]
        |> List.exists (fun x -> pixelVal block x y)
    ) 
    |> List.max
    |> (+) 1

let private isValidInBlock (block : Grid) offsX offsY x y =
    let size = gridSize block
    (x - offsX) >= 0 && (x - offsX) < size.Width &&
    (y - offsY) >= 0 && (y - offsY) < size.Height

let combine (comb : Combination) =
    let size = gridSize comb.Grid

    [0..size.Height - 1]
    |> List.map (fun y ->
        [0..size.Width - 1]
        |> List.map (fun x ->
            if isValidInBlock comb.Block comb.X comb.Y x y then
                pixelVal comb.Grid x y || pixelVal comb.Block (x - comb.X) (y - comb.Y)
            else
                pixelVal comb.Grid x y
            |> fun pixel -> { Pixel = pixel } 
        )
        |> fun pixels -> { Pixels = pixels }
    )
    |> fun rows -> { Rows = rows }

let canCombine (comb : Combination) =
    let size = gridSize comb.Grid
    let offsetValid =  
        comb.X + blockStartX comb.Block >= 0 && 
        comb.Y + blockStartY comb.Block >= 0 &&
        (comb.X + blockEndX comb.Block) <= size.Width && 
        (comb.Y + blockEndY comb.Block) <= size.Height

    let combinationValid = 
        [0..size.Height - 1]
        |> List.forall (fun y ->
            [0..size.Width - 1]
            |> List.forall (fun x ->
                if isValidInBlock comb.Block comb.X comb.Y x y then
                    not (pixelVal comb.Grid x y && pixelVal comb.Block (x - comb.X) (y - comb.Y))
                else
                    true
            )
        )
    
    offsetValid && combinationValid

let centerOffsetX (grid : Grid) (block : Grid) = ((gridSize grid).Width - (gridSize block).Width) / 2
