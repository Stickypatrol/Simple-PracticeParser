module ParserMonad

open ErrorMonad
open BaseTypes

type Parser<'char, 'a> = List<'char> -> Result<'a * List<'char>>

let ret (a:'a) : Parser<'char, 'a> = fun s -> err{return (a, s)}

let bind (p : Parser<'char, 'a>) (k: 'a -> Parser<'char, 'b>) : Parser<'char, 'b> =
  fun s ->
    err{
      let! a, s' = p s
      return! k a s'
    }

let (>>=) = bind

let fail failmsg =
  fun s ->
    ErrorMonad.Error failmsg

type ParserBuilder() =
  member this.ReturnFrom(a) = a
  member this.Return(a)= ret a
  member this.Bind (p, k) = p >>= k
let prs = ParserBuilder()

let (<|>) p1 p2 =
  fun s ->
    match p1 s with
    | Error err1 -> match p2 s with
                    | Error err2 -> Error (List.append err1 err2)
                    | Result res -> Result res
    | Result res -> Result res

let rec repeat (p:Parser<'char, 'a>) =
  prs{
    let! h = p
    let! t = repeat p
    return h::t
  }<|>
  prs{
    return []
  }

let repeatAtLeastOnce p =
  prs{
    let! h = p
    let! t = repeat p
    return h::t
  }