module Lexer

open ParserMonad
open ErrorMonad
open Base

let getHead =
  fun s ->
    match s with
    | h::t -> err{return (h,t)}
    | [] -> err{return! ErrorMonad.Error(["expected head, instead got tail"])}

let getEOF =
  fun s ->
    match s with
    | [] -> err{return ((), [])}
    | h::T -> err{return! ErrorMonad.Error(["expected EOF, instead got symbol"])}

let checkAlpha : Parser<char, char> =
  prs{
    let! result = getHead
    if (result >= 'a' && result <= 'z') || (result >= 'A' && result <= 'Z') then
      return result
    else
      return! fail ["expected alphabetic, got this: " + result.ToString()]
  }

let checkNumeric : Parser<char, char> =
  prs{
    let! result = getHead
    if (result >= '0' && result <= '9') then
      return result
    else
      return! fail ["expected numeric, got this: " + result.ToString()   ]
  }

let ReadInteger : Parser<char, Token> =
  prs{
    let! result = repeatAtLeastOnce checkNumeric
    return ValueType(Integer(System.Int32.Parse(List.fold (fun acc elem -> acc + (elem.ToString())) "" result)))
  }

let ReadString : Parser<char, Token> =
  prs{
    let! result = repeatAtLeastOnce checkNumeric
    return ValueType(String(List.fold (fun acc elem -> acc + (elem.ToString())) "" result))
  }


let lexer() =