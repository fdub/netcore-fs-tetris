module Fdub.Tetris.Display

open Fdub.Tetris.Model

let private renderPixel = function | false -> "  " | true -> "██"
let private renderRow (pixels : Pixel list) =
    pixels
    |> List.fold (fun result pixel -> result + (renderPixel pixel.Pixel)) ""

let show grid =
    System.Console.Clear()

    printfn "  %O  " ("==" |> String.replicate (grid.Rows.[0].Pixels |> List.length))
    grid.Rows
    |> List.iter (fun row ->
        printfn "||%O||" (renderRow row.Pixels)
    )
    printfn "  %O  " ("==" |> String.replicate (grid.Rows.[0].Pixels |> List.length))
