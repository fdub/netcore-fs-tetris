module Fdub.Tetris.Blocks

open Fdub.Tetris.Model

let private createBlock grid =
    grid 
    |> List.map (fun row ->
        row 
        |> List.map (fun pixel -> { Pixel = pixel > 0 })
        |> fun pixels -> { Pixels = pixels }
    )
    |> fun rows -> { Rows = rows }

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
