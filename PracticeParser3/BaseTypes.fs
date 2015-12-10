module BaseTypes

type ValueType =
  | String of string
  | Integer of int
  | Bool of bool

type Operator =
  | Assign
  | Plus
  | Minus
  | GreaterThan
  | SmallerThan
  | Equals
  | Multiply
  | Divide

type Curly = 
  | Open
  | Closed

type Round =
  | Open
  | Closed

type Bracket =
  | Curly of Curly
  | Round of Round

type Symbol =
  | Operator of Operator
  | Semicolon
  | Bracket of Bracket

type Keyword =
  | While
  | If
  | Else

type Function = //implement this later, it's low priority
  | Print
  | Parse

type Token =
  | Keyword of Keyword
  | Symbol of Symbol
  | ValueType of ValueType
  | Function of Function
  
type Block = List<Token>

type BlockTree =
  | Branch of List<BlockTree>
  | Leaf of Block