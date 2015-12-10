// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Lexer
open ErrorMonad

[<EntryPoint>]
let main argv = 
    let rec recurprint x =
      match x with
      | h::t -> printfn "%A" h
                recurprint t
      | [] -> ()
    let file = System.IO.File.ReadAllText "main.txt"
    match lexer() (List.ofSeq file) with
      | Result x -> printfn "%A" x
      | Error x ->  printfn "Something went wrong, we got this back:"
                    recurprint x

    0 // return an integer exit code