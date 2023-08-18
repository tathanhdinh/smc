module Parser

open Lexer
open AbstractSyntax

open FSharpPlus

exception ParsingError of string

let no_more_tokens () = raise (ParsingError "no more tokens")

(*
    primary-expression:
        identifier
        constant
        string-literal
        ( expression )
        generic-selection
*)
let rec primary_expression (tokens: TokenWithPosition list) =
    match tokens with
    | { Token = Token.Identifier i } as t :: ts -> ts, Identifier (i, t)

    | { Token = Token.Constant c } as t :: ts -> ts, Constant (c, t)

    | { Token = Token.Punctuator "(" } :: ts ->
        let ts, e = expression ts

        match ts with
        | { Token = Token.Punctuator ")" } :: ts -> ts, e

        | { Position = pos } :: _ -> error_at (pos, "expected )")
        | _ -> no_more_tokens ()

    | { Position = pos } :: _ -> error_at (pos, "expected primary expression")
    | _ -> no_more_tokens ()

(*
    unary-expression:
        postfix-expression
        ++ unary-expression
        -- unary-expression
        unary-operator cast-expression
        sizeof unary-expression
        sizeof ( type-name )
        _Alignof ( type-name )
*)
and unary_expression tokens =
    match tokens with
    | { Token = Token.Punctuator "+" } :: ts -> unary_expression ts
    | { Token = Token.Punctuator "-" } as t :: ts ->
        let ts, expr = unary_expression ts
        ts, Negation (expr, t)

    | { Token = Token.Punctuator "&" } as t :: ts ->
        let ts, expr = unary_expression ts
        ts, Address (expr, t)

    | { Token = Token.Punctuator "*" } as t :: ts ->
        let ts, expr = unary_expression ts
        ts, Dereference (expr, t)

    | _ -> primary_expression tokens

(*
    multiplicative-expression:
        cast-expression
        multiplicative-expression * cast-expression
        multiplicative-expression / cast-expression
        multiplicative-expression % cast-expression
*)
and multiplicative_expression tokens =
    let rec loop ts expr =
        match ts with
        | { Token = Token.Punctuator "*" } as t :: ts ->
            let ts, rhs = unary_expression ts
            loop ts (Binary (Mul, expr, rhs, t))


        | { Token = Token.Punctuator "/" } as t :: ts ->
            let ts, rhs = unary_expression ts
            loop ts (Binary (Div, expr, rhs, t))

        | _ -> ts, expr

    let ts, expr = unary_expression tokens
    loop ts expr

(*
    additive-expression:
        multiplicative-expression
        additive-expression + multiplicative-expression
        additive-expression - multiplicative-expression
*)
and additive_expression tokens =
    let rec loop ts expr =
        match ts with
        | { Token = Token.Punctuator "+" } as t :: ts ->
            let ts, rhs = multiplicative_expression ts
            loop ts (Binary (Add, expr, rhs, t))

        | { Token = Token.Punctuator "-" } as t :: ts ->
            let ts, rhs = multiplicative_expression ts
            loop ts (Binary (Sub, expr, rhs, t))

        | _ -> ts, expr

    let ts, expr = multiplicative_expression tokens
    loop ts expr

(*
    relational-expression :
        additive-expression
        relational-expression < additive-expression
        relational-expression > additive-expression
        relational-expression <= additive-expression
        relational-expression >= additive-expression
*)
and relational_expression tokens =
    let rec loop ts expr =
        match ts with
        | { Token = Token.Punctuator "<" } as t :: ts ->
            let ts, rhs = additive_expression ts
            loop ts (Binary (Lt, expr, rhs, t))

        | { Token = Token.Punctuator ">" } as t :: ts ->
            let ts, rhs = additive_expression ts
            loop ts (Binary (Gt, expr, rhs, t))

        | { Token = Token.Punctuator "<=" } as t :: ts ->
            let ts, rhs = additive_expression ts
            loop ts (Binary (Le, expr, rhs, t))

        | { Token = Token.Punctuator ">=" } as t :: ts ->
            let ts, rhs = additive_expression ts
            loop ts (Binary (Ge, expr, rhs, t))

        | _ -> ts, expr

    let ts, expr = additive_expression tokens
    loop ts expr

(*
    equality-expression:
        relational-expression
        equality-expression == relational-expression
        equality-expression != relational-expression
*)
and equality_expression tokens =
    let rec loop ts expr =
        match ts with
        | { Token = Token.Punctuator "==" } as t :: ts ->
            let ts, rhs = relational_expression ts
            loop ts (Binary (Eq, expr, rhs, t))

        | { Token = Token.Punctuator "!=" } as t :: ts ->
            let ts, rhs = relational_expression ts
            loop ts (Binary (Ne, expr, rhs, t))

        | _ -> ts, expr

    let ts, expr = relational_expression tokens
    loop ts expr

(*
    assignment-expression:
        conditional-expression
        unary-expression assignment-operator assignment-expression
*)
and assignment_expression tokens =
    let rec loop ts expr =
        match ts with
        | { Token = Token.Punctuator "=" } as t :: ts ->
            let ts, rhs = assignment_expression ts
            ts, Assignment (expr, rhs, t)

        | _ -> ts, expr

    let ts, expr = equality_expression tokens
    loop ts expr

(*
    expression:
        assignment-expression
        expression , assignment-expression
*)
and expression tokens = assignment_expression tokens

(*
    expression-statement:
        expression? ;
*)
and expression_statement tokens =
    match tokens with
    | { Token = Token.Punctuator ";" } as t :: ts -> ts, Expression (None, t)
    | _ ->
        let ts, expr = expression tokens

        match ts with
        | { Token = Token.Punctuator ";" } as t :: ts -> ts, Expression (Some expr, t)

        | { Position = pos } :: _ -> error_at (pos, "expected ;")
        | _ -> no_more_tokens ()

(*
    statement:
        expression-statement
        compound-statement
        selection-statement
        iteration-statement
        jump-statement
*)
and statement tokens =
    match tokens with
    | { Token = Token.Keyword "return" } :: ts ->
        let ts, expr = expression ts

        match ts with
        | { Token = Token.Punctuator ";" } as t :: ts -> ts, Return (Some expr, t)

        | { Position = pos } :: _ -> error_at (pos, "expected ;")
        | _ -> no_more_tokens ()

    | { Token = Token.Keyword "if" } as t :: ts ->
        let ts, cond = expression ts
        let ts, then_ = statement ts

        match ts with
        | { Token = Token.Keyword "else" } :: ts ->
            let ts, else_ = statement ts
            ts, If (cond, then_, Some else_, t)

        | _ -> ts, If (cond, then_, None, t)

    | { Token = Token.Keyword "for" } as t :: ts ->
        match ts with
        | { Token = Token.Punctuator "(" } :: ts ->
            let ts, init =
                match ts with
                | { Token = Token.Punctuator ";" } :: ts -> ts, None
                | _ ->
                    let ts, init = expression ts
                    ts, Some init

            let ts, cond =
                match ts with
                | { Token = Token.Punctuator ";" } :: ts -> ts, None
                | _ ->
                    let ts, cond = expression ts
                    ts, Some cond

            let ts, step =
                match ts with
                | { Token = Token.Punctuator ")" } :: ts -> ts, None
                | _ ->
                    let ts, step = expression ts
                    ts, Some step

            let ts, body = statement ts
            ts, For (init, cond, step, body, t)

        | { Position = pos } :: _ -> error_at (pos, "expected (")
        | _ -> no_more_tokens ()

    | { Token = Token.Keyword "while" } as t :: ts ->
        match ts with
        | { Token = Token.Punctuator "(" } :: ts ->
            let ts, cond = expression ts

            match ts with
            | { Token = Token.Punctuator ")" } :: ts ->
                let ts, body = statement ts
                ts, While (cond, body, t)

            | { Position = pos } :: _ -> error_at (pos, "expected )")
            | _ -> no_more_tokens ()

        | { Position = pos } :: _ -> error_at (pos, "expected (")
        | _ -> no_more_tokens ()

    | { Token = Token.Keyword "do" } as t :: ts ->
        let ts, body = statement ts

        match ts with
        | { Token = Token.Keyword "while" } :: ts ->
            match ts with
            | { Token = Token.Punctuator "(" } :: ts ->
                let ts, cond = expression ts

                match ts with
                | { Token = Token.Punctuator ")" } :: ts ->
                    let ts, _ = statement ts
                    ts, Do (body, cond, t)

                | { Position = pos } :: _ -> error_at (pos, "expected )")
                | _ -> no_more_tokens ()

            | { Position = pos } :: _ -> error_at (pos, "expected (")
            | _ -> no_more_tokens ()

        | { Position = pos } :: _ -> error_at (pos, "expected while")
        | _ -> no_more_tokens ()

    | { Token = Token.Punctuator "{" } as t :: ts ->
        let rec loop tokens stmts =
            match tokens with
            | { Token = Token.Punctuator "}" } :: ts -> ts, rev stmts

            | _ ->
                let ts, stmt = statement tokens
                loop ts (stmt :: stmts)

        let ts, stmts = loop ts []
        ts, Compound (stmts, t)

    | _ -> expression_statement tokens

let parse tokens =
    // let rec loop statements tokens =
    //     match tokens with
    //     | [] -> statements
    //     | _ ->
    //         let ts, stmts = statement tokens
    //         loop (stmts :: statements) ts

    // loop [] tokens |> List.rev

    let ts, stmts = statement tokens

    match ts with
    | [] -> stmts
    | { Position = pos } :: _ -> error_at (pos, "unexpected token")
