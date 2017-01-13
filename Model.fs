module Fdub.Tetris.Model

type Point = { X : int; Y : int; }
type Size = { Width : int; Height : int; }

type Pixel = { Value : bool }
type Row = { Pixels : Pixel list }
type Grid = { Rows : Row list; Size : Size }

let toPoint x y = { X = x; Y = y }
let toSize width height = { Width = width; Height = height }

let value (grid : Grid) (point : Point) = grid.Rows.[point.Y].Pixels.[point.X].Value

let private size (rows : Row list) = toSize (List.length rows.[0].Pixels) (List.length rows)

let createGrid (data : bool list list) =
    data 
    |> List.map (fun row ->
        row 
        |> List.map (fun pixel -> { Value = pixel })
        |> fun pixels -> { Pixels = pixels }
    )
    |> fun rows -> { Rows = rows; Size = size rows }

let initGrid width height =
    seq { 
        for r in [0..height - 1] do
        yield 
            seq {
                for c in [0..width - 1] do
                yield false
            }
            |> List.ofSeq
    }
    |> List.ofSeq
    |> createGrid

let (@+) (p1 : Point) (p2 : Point) = toPoint (p1.X + p2.X) (p1.Y + p2.Y)
let (@-) (p1 : Point) (p2 : Point) = toPoint (p1.X - p2.X) (p1.Y - p2.Y)
