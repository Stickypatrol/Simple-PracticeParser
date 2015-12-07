// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Lexer
open ErrorMonad

[<EntryPoint>]
let main argv = 
    let sample = "text to be checked"
    match lexer() (List.ofSeq sample) with
      | Result x -> printfn "%A" x
      | Error x -> printfn "%A" x
    0 // return an integer exit code