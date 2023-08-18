module AbstractSyntax

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

val show: Statement<'T> -> unit
