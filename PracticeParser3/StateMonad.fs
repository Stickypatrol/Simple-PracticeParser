module ParserMonad

open ErrorMonad

type Parser<'a, 'char> = List<'char> -> Result<'a * List<'char>>

let ret a = fun s -> (a, s)

let bind p k =
  fun s ->
    let a, s' = p s
    k a s'

let (>>=) = bind

type ParserBuilder() =
  member this.Return a = ret a
  member this.Bind p k = p >>= k
let prs = ParserBuilder()
