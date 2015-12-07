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

let readInteger : Parser<char, Token> =
  prs{
    let! result = repeatAtLeastOnce checkNumeric
    return ValueType(Integer(System.Int32.Parse(List.fold (fun acc elem -> acc + (elem.ToString())) "" result)))
  }

let readString : Parser<char, Token> =
  prs{
    let! result = repeatAtLeastOnce checkAlpha
    return ValueType(String(List.fold (fun acc elem -> acc + (elem.ToString())) "" result))
  }

let whiteSpace : Parser<char, Unit> =
  prs{
    let! result = getHead
    if result = '\t' || result = ' ' || result = '\n' || result = '\r' then
      return ()
    else
      return! fail ["expected whitespace"]
  }

let checkWhiteSpace : Parser<char, Unit> =
  prs{
    let! _ = repeat whiteSpace
    return ()
  }

let rec lexer () =
  prs{
    //here I want all the possible matches
    do! checkWhiteSpace
    let! head =
      readString <|>
      readInteger
    do! checkWhiteSpace
    let! tail = lexer()
    return head::tail
  }<|>
  prs{
    do! getEOF
    return []
  }