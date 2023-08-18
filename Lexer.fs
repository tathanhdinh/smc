module Lexer

open FSharpPlus

type Token =
    | Keyword of string
    | Identifier of string
    | Constant of int
    | Punctuator of string

type Position = { Line: int; Column: int }

type TokenWithPosition = { Token: Token; Position: Position }

let mutable source_file, source_code = "", ""
let mutable source_code_lines = [||]

let find_position (abspos: int) =
    let mutable i, line_num, line_start_offset = 0, 0, 0

    while i < abspos do
        if source_code.[i] = '\n' then
            line_start_offset <- i + 1
            line_num <- line_num + 1

        i <- i + 1

    { Line = line_num
      Column = abspos - line_start_offset }

let error_at (pos, msg) =
    let esc = $"%c{char 0x1b}"

    let line, pad, msg =
        source_code_lines[pos.Line], String.replicate (pos.Column) " ", "^ " + esc + "[31;1m" + msg + esc + "[0m"

    failwith $"{source_file}:%d{pos.Line}: \n{line}\n{pad}{msg}"

let punctuators =
    [ "..."; "&&"; "-="; ">="; "~"; "+"; ";"; "]"; "<<="; "&="; "->"; ">>"; "%"; ","; "<"; "^" ]
    @ [ ">>="; "*="; "/="; "^="; "&"; "-"; "="; "{"; "!="; "++"; "<<"; "|="; "("; "."; ">"; "|" ]
    @ [ "%="; "+="; "<="; "||"; ")"; "/"; "?"; "}"; "##"; "--"; "=="; "!"; "*"; ":"; "["; "#" ]

let keywords = [ "return"; "if"; "else"; "for"; "while"; "int" ]

let (|WhiteSpaces|_|) (s: string) =
    let len = length s
    let mutable i = 0

    while i < len && System.Char.IsWhiteSpace s[i] do
        i <- i + 1

    if i > 0 then Some s[.. (i - 1)] else None

let (|AnyOf|_|) (prefixes: _ list) (s: string) =
    let ps =
        prefixes |> filter (fun (x: string) -> s.StartsWith x) |> List.sortDescending

    match ps with
    | [] -> None
    | p :: _ -> Some p

let (|Identifier|_|) (s: string) =
    let len = length s

    if len = 0 || not (System.Char.IsAsciiLetter s[0]) then
        None
    else
        let mutable i = 1

        while i < len && System.Char.IsAsciiLetterOrDigit s[i] do
            i <- i + 1

        Some s[.. (i - 1)]

let (|Constant|_|) (s: string) =
    let len = length s
    let mutable i = 0

    while i < len && System.Char.IsAsciiDigit s[i] do
        i <- i + 1

    if i = 0 then None else Some s[.. (i - 1)]

let lex input =
    source_file <- input
    source_code <- System.IO.File.ReadAllText input
    source_code_lines <- source_code.Split [| '\n' |]

    let l = length source_code
    let mutable abspos, tokens = 0, []

    while abspos < l do
        let token, nextpos =
            match source_code[abspos..] with
            | WhiteSpaces blank -> None, abspos + length blank

            | AnyOf punctuators punct -> Some (Token.Punctuator punct), abspos + length punct

            | Identifier word ->
                if List.contains word keywords then
                    Some (Token.Keyword word), abspos + word.Length
                else
                    Some (Token.Identifier word), abspos + word.Length

            | Constant num -> Some (Token.Constant (int num)), abspos + length num

            | _ -> error_at (find_position abspos, "invalid token")

        match token with
        | Some token ->
            tokens <-
                { Token = token
                  Position = find_position abspos }
                :: tokens
        | None -> ()

        abspos <- nextpos

    rev tokens
