module ErrorMonad

type Result<'a> =
  | Result of 'a
  | Error of List<string>

let ret a = Result(a)

let bind p k =
  match p with
  | Result x -> k x
  | Error x -> Error x
let (>>=) = bind

type ResultBuilder() =
  member this.Return a = ret a
  member this.ReturnFrom a = a
  member this.Bind (p,k) = p >>= k

let err = ResultBuilder()