module IntermediateRepresentation

type Expression =
    | Label of string
    | Temporary of string
    | Memory of Expression
    | Sequence of Expression * Expression
