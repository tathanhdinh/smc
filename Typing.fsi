module Typing

open Lexer
open AbstractSyntax

[<RequireQualifiedAccess>]
type Type =
    | Unit
    | Int
    | Ptr of Type

val inference: Statement<TokenWithPosition> -> Statement<TokenWithPosition * Type>
