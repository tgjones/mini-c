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
    | Expression of Expression
    | Nop

and CompoundStatement = LocalDeclarations option * Statement list

and LocalDeclarations = LocalDeclaration list

and LocalDeclaration =
    | ScalarDeclaration of TypeSpec * Identifier
    | ArrayDeclaration of TypeSpec * Identifier

and Expression =
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | LiteralExpression of Literal

and BinaryOperator =
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreaterEqual
    | Greater
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus

and UnaryOperator =
    | LogicalNegate
    | Negate
    | Identity

and Literal =
    | BoolLiteral of bool
    | IntLiteral of int
    | FloatLiteral of float