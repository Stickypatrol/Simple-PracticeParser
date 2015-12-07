// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Lexer
open ErrorMonad

[<EntryPoint>]
let main argv = 
    let sample = "aaaa    aa11aa    1234567890"
    match lexer() (List.ofSeq sample) with
      | Result x -> printfn "%A" x
      | Error x -> printfn "%A" x
    0 // return an integer exit code