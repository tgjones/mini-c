module MiniC.Compiler.Ast

type IDeclaration = interface end

type Program = Declaration list

and Declaration =
    | VariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and TypeSpec =
    | Void
    | Bool
    | Int
    | Float

and VariableDeclaration = 
    | ScalarVariableDeclaration of TypeSpec * Identifier
    | ArrayVariableDeclaration of TypeSpec * Identifier

and FunctionDeclaration = TypeSpec * Identifier * Parameters * CompoundStatement

and Identifier = string

and Parameters = Parameter list

and Parameter =
    | ScalarParameter of TypeSpec * Identifier
    | ArrayParameter of TypeSpec * Identifier

and Statement =
    | ExpressionStatement of ExpressionStatement
    | CompoundStatement of CompoundStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement

and ExpressionStatement =
    | Expression of Expression
    | Nop

and CompoundStatement = LocalDeclarations * Statement list

and LocalDeclarations = LocalDeclaration list

and LocalDeclaration =
    | ScalarLocalDeclaration of TypeSpec * Identifier
    | ArrayLocalDeclaration of TypeSpec * Identifier

and IfStatement = Expression (* condition *) * Statement (* then *) * Statement option (* else *)

and WhileStatement = Expression * Statement

and Expression =
    | AssignmentExpression of AssignmentExpression
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | IdentifierExpression of Identifier
    | ArrayIdentifierExpression of Identifier * Expression
    | FunctionCallExpression of Identifier * Arguments
    | ArraySizeExpression of Identifier
    | LiteralExpression of Literal
    | ArrayAllocationExpression of TypeSpec * Expression

and AssignmentExpression =
    | ScalarAssignmentExpression of Identifier * Expression
    | ArrayAssignmentExpression of Identifier * Expression * Expression

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

and Arguments = Expression list

and Literal =
    | BoolLiteral of bool
    | IntLiteral of int
    | FloatLiteral of float