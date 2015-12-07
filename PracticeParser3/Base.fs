module Base

type Keyword = string

type ValueType =
  | String of string
  | Integer of int

type Token =
  | Keyword of Keyword
  | ValueType of ValueType