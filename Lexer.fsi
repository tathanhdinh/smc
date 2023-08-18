module Lexer

[<RequireQualifiedAccess>]
type Token =
    | Keyword of string
    | Identifier of string
    | Constant of int
    | Punctuator of string

type Position = { Line: int; Column: int }

type TokenWithPosition = { Token: Token; Position: Position }

val error_at: Position * string -> 'a

val lex: string -> TokenWithPosition list
