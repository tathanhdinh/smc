module AbstractSyntax

open FSharpPlus

type BinaryOperator =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge

type Expression<'T> =
    | Constant of int * 'T
    | Identifier of Ident: string * 'T
    | Negation of Expr: Expression<'T> * 'T
    | Address of Expr: Expression<'T> * 'T
    | Dereference of Expr: Expression<'T> * 'T
    | Binary of Op: BinaryOperator * Lhs: Expression<'T> * Rhs: Expression<'T> * 'T
    | Assignment of Lhs: Expression<'T> * Rhs: Expression<'T> * 'T


type Statement<'T> =
    | Expression of Expression<'T> option * 'T
    | Compound of Stmts: Statement<'T> list * 'T
    | Return of Expression<'T> option * 'T
    | If of Cond: Expression<'T> * Then: Statement<'T> * Else: Statement<'T> option * 'T
    | For of
        Init: Expression<'T> option *
        Cond: Expression<'T> option *
        Step: Expression<'T> option *
        Body: Statement<'T> *
        'T
    | While of Cond: Expression<'T> * Body: Statement<'T> * 'T
    | Do of Body: Statement<'T> * Cond: Expression<'T> * 'T

(*
    Kennedy, Andrew. Functional Pearl: Drawing Trees.
    Journal of Functional Programming. Volume 6. 1996. pp. 527-534.
*)

let binop_to_string (op: BinaryOperator) =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="

type Tree<'T> = Tree of Support: 'T * Subtrees: Tree<'T> list

let rec expression_to_tree expr =
    match expr with
    | Constant (value, _) -> Tree ($"%d{value}", [])

    | Identifier (ident, _) -> Tree (ident, [])

    | Negation (expr, _) -> Tree ("-", [ expression_to_tree expr ])

    | Address (expr, _) -> Tree ("addrof", [ expression_to_tree expr ])

    | Dereference (expr, _) -> Tree ("deref", [ expression_to_tree expr ])

    | Binary (op, lhs, rhs, _) -> Tree (binop_to_string op, [ expression_to_tree lhs; expression_to_tree rhs ])

    | Assignment (lhs, rhs, _) -> Tree ("=", [ expression_to_tree lhs; expression_to_tree rhs ])

let rec statement_to_tree stmt =
    match stmt with
    | Expression (expr, _) ->
        Tree (
            expr
            |> Option.map (fun e ->
                match e with
                | Constant _ -> "constant"
                | Identifier _ -> "identifier"
                | Negation _ -> "unary expression"
                | Address _ -> "unary expression"
                | Dereference _ -> "unary expression"
                | Binary _ -> "binary expression"
                | Assignment _ -> "assign")
            |> Option.defaultValue "null statement",
            Option.map expression_to_tree expr |> Option.toList
        )

    | Compound (stmts, _) -> Tree ("{...}", stmts |> List.map statement_to_tree)

    | Return (expr, _) -> Tree ("return", Option.map expression_to_tree expr |> Option.toList)

    | If (cond, then_, else_, _) ->
        let cond_tree =
            match expression_to_tree cond with
            | Tree (_, cond_subtrees) -> Tree ("condition", cond_subtrees)

        let then_tree =
            match statement_to_tree then_ with
            | Tree (_, then_subtrees) -> Tree ("then", then_subtrees)

        let else_tree =
            Option.map statement_to_tree else_
            |> Option.map (fun else_tree ->
                match else_tree with
                | Tree (_, else_subtrees) -> Tree ("else", else_subtrees))

        Tree ("if", [ expression_to_tree cond; then_tree ] @ Option.toList else_tree)

    // match Option.map statement_to_tree else_ with
    // | Some (Tree (_, else_subtrees)) -> Some (Tree ("else", else_subtrees))
    // | None -> None

    // let Tree (_, cond_subtrees) = expression_to_tree cond
    // let cond_tree = Tree("condition", cond_subtrees)

    // Tree (
    //     "if",
    //     [ expression_to_tree cond; statement_to_tree then_ ]
    //     @ (Option.map statement_to_tree else_ |> Option.toList)
    // )

    | For (init, cond, step, body, _) ->
        Tree (
            "for",
            ([ init; cond; step ] |> List.choose id |> List.map expression_to_tree)
            @ [ statement_to_tree body ]
        )

    | While (cond, body, _) -> Tree ("while", [ expression_to_tree cond; statement_to_tree body ])

    | Do (body, cond, _) -> Tree ("do", [ statement_to_tree body; expression_to_tree cond ])

type Extent = list<{| Left: float; Right: float |}>

let move (e: Extent, x) : Extent =
    e
    |> map (fun a ->
        {| Left = a.Left + x
           Right = a.Right + x |})

let merge es =
    let rec merge' (e0: Extent) (e1: Extent) =
        match e0, e1 with
        | [], _ -> e1
        | _, [] -> e0
        | a :: e0', b :: e1' -> {| Left = a.Left; Right = b.Right |} :: merge' e0' e1'

    fold merge' [] es

let rec fit' (e0: Extent) (e1: Extent) =
    match e0, e1 with
    | [], _
    | _, [] -> 0.0
    | _, _ ->
        let r = e0 |> map (fun a -> a.Right) |> Seq.max
        let l = e1 |> map (fun a -> a.Left) |> Seq.min
        r - l + 4.0

// match e0, e1 with
// | a :: e0', b :: e1' -> max (a.right - b.left + 3.0) (fit' e0' e1')
// | _ -> 0.0

let fitl es =
    let rec fitl' acc es =
        match es with
        | [] -> []
        | e :: es' ->
            let x = fit' acc e
            x :: fitl' (merge [ acc; move (e, x) ]) es'

    fitl' [] es

let fitr es =
    let rec fitr' acc es =
        match es with
        | [] -> []
        | e :: es' ->
            let x = -(fit' e acc)
            x :: fitr' (merge [ move (e, x); acc ]) es'

    es |> rev |> fitr' [] |> rev

let fit es =
    zip (fitl es) (fitr es) |> map (fun (a, b) -> (a + b) / 2.0)

let design (tree: Tree<string>) : Tree<string * float> =
    let moveTree (Tree ((label, pos), subtrees), x) = Tree ((label, pos + x), subtrees)

    let rec design' (Tree (label, subtrees)) =
        let trees, extents = unzip (map design' subtrees)
        let positions = fit extents

        let ptrees = map moveTree (zip trees positions)
        let pextents = map move (zip extents positions)

        let l = float (length label) / 2.0

        let resultextent = {| Left = -l; Right = l |} :: (merge pextents)
        let resulttree = Tree ((label, 0.0), ptrees)

        (resulttree, resultextent)

    tree |> design' |> fst

let draw (tree: Tree<string * float>, pos) =
    let rec bfs tps lines =
        match tps with
        | [] -> rev lines
        | _ ->
            let mutable subtps, line = [], ""

            for Tree ((label, pos), subtrees), parent_abs_pos in tps do
                let l = (label |> length |> float) / 2.0
                let abs_pos = pos + parent_abs_pos

                line <- String.padRight (abs_pos - l |> int) line
                line <- line + label
                subtps <- subtps @ (map (fun t -> t, abs_pos) subtrees)

            bfs subtps (line :: lines)

    bfs [ (tree, pos) ] [] |> String.concat "\n\n" |> printfn "%s"

let rec minpos (Tree ((label, pos), subtrees)) =
    let p = pos - float (length label) / 2.0

    match subtrees with
    | [] -> p
    | _ -> min p (pos + (subtrees |> map minpos |> Seq.min))

let show (ast: Statement<'T>) =
    let tree = statement_to_tree ast |> design
    let pos = -(minpos tree)
    draw (tree, pos)
