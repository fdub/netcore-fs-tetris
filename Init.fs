module Fdub.Tetris.Init

open Fdub.Tetris.Model
let private createPixels num =
    [1..num] |> List.map (fun _ -> { Pixel = false })

let private createRows numPixels numRows =
    [1..numRows] |> List.map (fun _ -> { Pixels = createPixels numPixels })

let initGrid width height =
    { Rows = createRows width height }
