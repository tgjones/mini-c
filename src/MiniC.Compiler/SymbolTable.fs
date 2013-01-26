namespace MiniC.Compiler

open System.Collections.Generic
open Ast

type private SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<VariableDeclaration>
    let declaresIdentifier (identifierRef : IdentifierRef) =
        function
        | ScalarVariableDeclaration(t, i) -> i = identifierRef.Identifier
        | ArrayVariableDeclaration(t, i) -> i = identifierRef.Identifier

    member x.AddDeclaration declaration =
        list <- declaration :: list

    member x.FindDeclaration identifierRef =
        let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list
        match found with
        | Some(d) -> d
        | None ->
            match parent with
            | Some(ss) -> ss.FindDeclaration identifierRef
            | None -> failwithf "Could not find declaration of identifier %s" identifierRef.Identifier

type private SymbolScopeStack() =
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))

    member x.CurrentScope = stack.Peek()

    member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member x.Pop() = stack.Pop() |> ignore
    member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration

type SymbolTable(program) as self =
    inherit Dictionary<IdentifierRef, VariableDeclaration>(HashIdentity.Reference)

    let symbolScopeStack = new SymbolScopeStack()

    let rec scanDeclaration =
        function
        | StaticVariableDeclaration(x) -> symbolScopeStack.AddDeclaration x
        | FunctionDeclaration(x)       -> scanFunctionDeclaration x

    and scanFunctionDeclaration (_, _, parameters, compoundStatement) =
        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration
        scanCompoundStatement compoundStatement
        symbolScopeStack.Pop() |> ignore

    and scanCompoundStatement (localDeclarations, statements) =
        symbolScopeStack.Push()
        localDeclarations |> List.iter (fun d -> symbolScopeStack.AddDeclaration d)
        statements |> List.iter scanStatement
        symbolScopeStack.Pop() |> ignore

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

    and addIdentifierMapping identifierRef =
        let declaration = symbolScopeStack.CurrentScope.FindDeclaration identifierRef
        self.Add(identifierRef, declaration)

    and scanExpression expression =
        match expression with
        | AssignmentExpression(ae) ->
            match ae with
            | ScalarAssignmentExpression(i, e) ->
                addIdentifierMapping i
                scanExpression e
            | ArrayAssignmentExpression(i, e1, e2) ->
                addIdentifierMapping i
                scanExpression e1
                scanExpression e2
        | BinaryExpression(e1, _, e2) ->
            scanExpression e1
            scanExpression e2
        | UnaryExpression(_, e) ->
            scanExpression e
        | IdentifierExpression(i) ->
            addIdentifierMapping i
        | ArrayIdentifierExpression(i, e) ->
            addIdentifierMapping i
            scanExpression e
        | FunctionCallExpression(_, args) ->
            args |> List.iter scanExpression
        | ArraySizeExpression(i) ->
            addIdentifierMapping i
        | LiteralExpression(l) -> ()
        | ArrayAllocationExpression(_, e) ->
            scanExpression e

    do program |> List.iter scanDeclaration