module MiniC.Compiler.SemanticAnalysis

open System.Collections.Generic
open Ast
open AstUtilities

type ExpressionTypeDictionary = Dictionary<Expression, System.Type>

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeDictionary;
    }

let typeOfIdentifier (symbolTable : SymbolTable) identifier =
    let declaration = symbolTable.[identifier]
    typeOfVariableDeclaration declaration

let rec typeOfExpression symbolTable =
    function
    | Ast.AssignmentExpression(x) ->
        match x with
        | Ast.ScalarAssignmentExpression(i, _)
        | Ast.ArrayAssignmentExpression(i, _, _) ->
            typeOfIdentifier symbolTable i
    | Ast.BinaryExpression(l, op, r) ->
        typeOfExpression symbolTable l // TODO: Might need to "widen" from int to float
    | Ast.UnaryExpression(op, e) ->
        typeOfExpression symbolTable e // TODO: Might need to "widen" from int to float