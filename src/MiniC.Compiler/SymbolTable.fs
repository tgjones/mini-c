namespace MiniC.Compiler

open System.Collections.Generic
open Ast

type SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<IVariableDeclaration>
    let declaresIdentifier identifier (declaration : IVariableDeclaration) =
        match declaration with
        | :? VariableDeclaration as d ->
            match d with
            | ScalarVariableDeclaration(t, i) -> i = identifier
            | ArrayVariableDeclaration(t, i) -> i = identifier
        | :? Parameter as d ->
            match d with
            | ScalarParameter(t, i) -> i = identifier
            | ArrayParameter(t, i) -> i = identifier
        | :? LocalDeclaration as d ->
            match d with
            | ScalarLocalDeclaration(t, i) -> i = identifier
            | ArrayLocalDeclaration(t, i) -> i = identifier
        | _ -> failwith "Oops"

    member x.AddDeclaration declaration =
        list <- declaration :: list

    member x.FindDeclaration identifier =
        let found = List.tryFind (fun x -> declaresIdentifier identifier x) list
        match found with
        | Some(d) -> d
        | None ->
            match parent with
            | Some(ss) -> ss.FindDeclaration identifier
            | None -> failwithf "Could not find declaration of identifier %s" identifier

type SymbolEnvironment = Dictionary<Expression, SymbolScope>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SymbolEnvironment =
    type SymbolTable() =
        let stack = new Stack<SymbolScope>()
        do stack.Push(new SymbolScope(None))

        member x.CurrentScope = stack.Peek()

        member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
        member x.Pop() = stack.Pop() |> ignore
        member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration

    let create program =
        let result = new SymbolEnvironment(HashIdentity.Reference)
        let symbolTable = new SymbolTable()

        let rec scanDeclaration =
            function
            | VariableDeclaration(x) -> symbolTable.AddDeclaration x
            | FunctionDeclaration(x) -> scanFunctionDeclaration x

        and scanFunctionDeclaration (_, _, parameters, compoundStatement) =
            symbolTable.Push()
            parameters |> List.iter symbolTable.AddDeclaration
            scanCompoundStatement compoundStatement
            symbolTable.Pop() |> ignore

        and scanCompoundStatement (localDeclarations, statements) =
            symbolTable.Push()
            localDeclarations |> List.iter (fun d -> symbolTable.AddDeclaration d)
            statements |> List.iter scanStatement
            symbolTable.Pop() |> ignore

        and scanStatement =
            function
            | ExpressionStatement(es) ->
                match es with
                | Expression(e) -> scanExpression e
                | Nop -> ()
            | CompoundStatement(x) -> scanCompoundStatement x
            | IfStatement(e, s1, Some(s2)) ->
                scanExpression e
                scanStatement s1
                scanStatement s2
            | IfStatement(e, s1, None) ->
                scanExpression e
                scanStatement s1
            | WhileStatement(e, s) ->
                scanExpression e
                scanStatement s
            | ReturnStatement(Some(e)) ->
                scanExpression e
            | _ -> ()

        and scanExpression expression =
            match expression with
            | AssignmentExpression(ae) ->
                match ae with
                | ScalarAssignmentExpression(i, e) ->
                    scanExpression e
                | ArrayAssignmentExpression(i, e1, e2) ->
                    scanExpression e1
                    scanExpression e2
            | BinaryExpression(e1, _, e2) ->
                scanExpression e1
                scanExpression e2
            | UnaryExpression(_, e)
            | ArrayIdentifierExpression(_, e)
            | ArrayAllocationExpression(_, e) ->
                scanExpression e
            | _ -> ()
            result.Add(expression, symbolTable.CurrentScope)

        program |> List.iter scanDeclaration
        result

    let findDeclaration expression (symbolEnvironment : SymbolEnvironment) =
        let identifier =
            match expression with
            | AssignmentExpression(ae) ->
                match ae with
                | ScalarAssignmentExpression(i, _) -> i
                | ArrayAssignmentExpression(i, _, _) -> i
            | IdentifierExpression(i) -> i
            | ArrayIdentifierExpression(i, _) -> i
            | ArraySizeExpression(i) -> i
            | _ -> failwith "Expression doesn't include an identifier"

        let mutable symbolScope = new SymbolScope(None)
        if symbolEnvironment.TryGetValue(expression, &symbolScope) then
            symbolScope.FindDeclaration identifier
        else
            failwithf "Could not find expression in symbol environment"