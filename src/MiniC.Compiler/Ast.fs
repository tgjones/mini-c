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

and Parameters = Parameter list

and Parameter =
    | ScalarParameter of TypeSpec * Identifier
    | ArrayParameter of TypeSpec * Identifier

and Statement =
    | CompoundStatement of CompoundStatement
    | ExpressionStatement of ExpressionStatement

and ExpressionStatement =
    Nop

and CompoundStatement = LocalDeclarations option * Statement list

and LocalDeclarations = LocalDeclaration list

and LocalDeclaration =
    | ScalarDeclaration of TypeSpec * Identifier
    | ArrayDeclaration of TypeSpec * Identifier