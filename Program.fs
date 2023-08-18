[<EntryPoint>]
let main argv =
    let argc = argv.Length

    if argc = 0 then
        printfn "smc: no input files"
    else
        try
            let show_tokens = argc > 1 && argv.[1] = "-lex"
            let show_ast = argc > 1 && argv.[1] = "-parse"

            let tokens = Lexer.lex argv[0]

            if show_tokens then
                printfn "%A" tokens
            elif show_ast then
                tokens |> Parser.parse |> AbstractSyntax.show

        with Failure msg ->
            eprintf "%s" msg

    0
