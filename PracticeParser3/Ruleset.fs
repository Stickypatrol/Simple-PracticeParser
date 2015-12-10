module Ruleset
//this module contains the rules in the shape of discriminated unions that blocks are restricted to
//this is to make sure that expressions are always safe to execute
type Expression =
  | 