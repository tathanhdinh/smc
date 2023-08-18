module Parser

open Lexer
open AbstractSyntax

exception ParsingError of string

val parse: TokenWithPosition list -> Statement<TokenWithPosition>
