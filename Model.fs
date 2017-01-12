module Fdub.Tetris.Model

type Pixel = { Pixel : bool }
type Row = { Pixels : Pixel list }
type Grid = { Rows : Row list }
type Size = { Width : int; Height : int; }