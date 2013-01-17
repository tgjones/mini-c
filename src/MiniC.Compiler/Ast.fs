module MiniC.Compiler.Ast

type Program = Declaration list

and Declaration =
    | VariableDeclaration
    | FunctionDeclaration of TypeSpec * Identifier * Parameters option * CompoundStatement

and TypeSpec =
    | Void
    | Bool
    | Int
    | Float

and Identifier = string

and Parameters =
    | ParameterList
    | Void

and Statement =
    | CompoundStatement of CompoundStatement

and CompoundStatement = LocalDeclarations option * Statement list

and LocalDeclarations = LocalDeclaration list

and LocalDeclaration =
    | ScalarDeclaration of TypeSpec * Identifier
    | ArrayDeclaration of TypeSpec * Identifier