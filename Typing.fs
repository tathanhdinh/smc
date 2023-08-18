module Typing

open Lexer
open AbstractSyntax

open FSharpPlus

[<RequireQualifiedAccess>]
type Type =
    | Unit
    | Int
    | Ptr of Type

let type_environment = Map.empty

let rec is_lvalue expr =
    match expr with
    | Identifier (_, _)
    | Dereference (_, _) -> true
    | _ -> false

let typeof expr =
    match expr with
    | Constant (_, (_, typ)) -> typ
    | Identifier (_, (_, typ)) -> typ
    | Negation (_, (_, typ)) -> typ
    | Address (_, (_, typ)) -> typ
    | Dereference (_, (_, typ)) -> typ
    | Binary (_, _, _, (_, typ)) -> typ
    | Assignment (_, _, (_, typ)) -> typ

let rec type_expression expr =
    match expr with
    | Constant (constant, support) -> Constant (constant, (support, Type.Int))

    | Identifier (ident, support) ->
        match Map.tryFind ident type_environment with
        | Some typ -> Identifier (ident, (support, typ))
        | _ ->
            let { Token = _; Position = pos } = support
            error_at (pos, "undeclared identifier")

    | Negation (expr, support) ->
        let expr' = type_expression expr

        match typeof expr' with
        | Type.Int -> Negation (expr', (support, Type.Int))
        | _ ->
            let { Token = _; Position = pos } = support
            error_at (pos, "type mismatch")

    | Address (expr, support) ->
        if is_lvalue expr then
            let expr' = type_expression expr
            Address (expr', (support, Type.Ptr (typeof expr')))
        else
            let { Token = _; Position = pos } = support
            error_at (pos, "cannot take address of non-lvalue")

    | Dereference (expr, support) ->
        let expr' = type_expression expr

        match typeof expr' with
        | Type.Ptr typ -> Dereference (expr', (support, typ))
        | _ ->
            let { Token = _; Position = pos } = support
            error_at (pos, "dereference of non-pointer")

    | Assignment (lhs, rhs, support) ->
        if is_lvalue lhs then
            let lhs' = type_expression lhs
            let rhs' = type_expression rhs

            Assignment (lhs', rhs', (support, typeof lhs'))
        else
            let { Token = _; Position = pos } = support
            error_at (pos, "cannot assign to non-lvalue")

    | Binary (op, lhs, rhs, support) ->
        let lhs' = type_expression lhs
        let rhs' = type_expression rhs

        let typ =
            match op, typeof lhs', typeof rhs' with
            | _, Type.Int, Type.Int -> Type.Int

            | (Add | Sub), (Type.Ptr _ as ltyp), Type.Int -> ltyp

            | Sub, Type.Ptr ltyp, Type.Ptr rtyp when ltyp = rtyp -> Type.Int

            | _ ->
                let { Token = _; Position = pos } = support
                error_at (pos, "type mismatch")

        Binary (op, lhs', rhs', (support, typ))

let rec inference stmt =
    match stmt with
    | Expression (expr, support) ->
        let expr' = map type_expression expr
        Expression (expr', (support, Type.Unit))

    | Compound (stmts, support) ->
        let stmts' = map inference stmts
        Compound (stmts', (support, Type.Unit))

    | Return (expr, support) ->
        let expr' = map type_expression expr
        Return (expr', (support, Type.Unit))

    | If (cond, then_, else_, support) ->
        let cond' = type_expression cond
        let then_' = inference then_
        let else_' = map inference else_

        If (cond', then_', else_', (support, Type.Unit))

    | For (init, cond, step, body, support) ->
        let init' = map type_expression init
        let cond' = map type_expression cond
        let step' = map type_expression step
        let body' = inference body

        For (init', cond', step', body', (support, Type.Unit))

    | While (cond, body, support) ->
        let cond' = type_expression cond
        let body' = inference body

        While (cond', body', (support, Type.Unit))

    | Do (body, cond, support) ->
        let body' = inference body
        let cond' = type_expression cond

        Do (body', cond', (support, Type.Unit))
