module Fdub.Tetris.Input

open System
open System.Threading 
open System.Threading.Tasks

let private keyEvent = new AutoResetEvent(false)
let private queue = ref List.empty
let private cancellationTokenSource = new CancellationTokenSource()

let private dequeue () =
    match !queue |> List.rev with
    | h::t -> 
        queue := t |> List.rev
        Some h
    | _ -> 
        None

let private tick _ =
    keyEvent.Set() |> ignore

let private timer = 
    new Timer(
        TimerCallback(tick),
        (),
        TimeSpan.Zero,
        TimeSpan.FromMilliseconds 0.)

let rec private keyLoop () = 
    let key = Console.ReadKey(true)
    queue := key.Key :: !queue
    keyEvent.Set() |> ignore
    keyLoop ()

let init (timeout : float) = 
    Task.Run(keyLoop, cancellationTokenSource.Token) |> ignore
    timer.Change(TimeSpan.Zero, TimeSpan.FromMilliseconds timeout) |> ignore

let stop() = 
    cancellationTokenSource.Cancel()
    cancellationTokenSource.Dispose()
    timer.Dispose()
    keyEvent.Dispose()

let readKey () =
    match List.length !queue with
    | 0 -> 
        match keyEvent.WaitOne() with
        | true -> dequeue ()
        | false -> None
    | _ ->
        dequeue ()

let tickTimer (timeout : float) =
    timer.Change(
        TimeSpan.FromMilliseconds (timeout / 5.), 
        TimeSpan.FromMilliseconds timeout
    ) |> ignore
    