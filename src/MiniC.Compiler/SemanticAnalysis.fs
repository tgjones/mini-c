module MiniC.Compiler.SemanticAnalysis

open System.Collections.Generic
open CompilerErrors
open Ast

type private SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<VariableDeclaration>
    let identifierFromDeclaration =
        function
        | ScalarVariableDeclaration(_, i)
        | ArrayVariableDeclaration(_, i) -> i

    let declaresIdentifier (identifierRef : IdentifierRef) declaration =
        (identifierFromDeclaration declaration) = identifierRef.Identifier

    member x.AddDeclaration declaration =
        if List.exists (fun x -> identifierFromDeclaration x = identifierFromDeclaration declaration) list then
            raise (variableAlreadyDefined (identifierFromDeclaration declaration))
        list <- declaration :: list

    member x.FindDeclaration identifierRef =
        let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list
        match found with
        | Some(d) -> d
        | None ->
            match parent with
            | Some(ss) -> ss.FindDeclaration identifierRef
            | None -> raise (nameDoesNotExist (identifierRef.Identifier))

type private SymbolScopeStack() =
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))

    member x.CurrentScope = stack.Peek()

    member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member x.Pop() = stack.Pop() |> ignore
    member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration

type VariableType =
    {
        Type    : TypeSpec;
        IsArray : bool;
    }
    override x.ToString() =
        x.Type.ToString() + (if x.IsArray then "[]" else "")

let scalarType t = { Type = t; IsArray = false; }

type FunctionTableEntry =
    {
        ReturnType     : TypeSpec;
        ParameterTypes : VariableType list;
    }

let typeOfDeclaration =
    function
    | Ast.ScalarVariableDeclaration(t, _) -> { Type = t; IsArray = false }
    | Ast.ArrayVariableDeclaration(t, _)  -> { Type = t; IsArray = true }

type FunctionTable(program) as self =
    inherit Dictionary<Identifier, FunctionTableEntry>()

    let rec scanDeclaration =
        function
        | StaticVariableDeclaration(x)    -> ()
        | FunctionDeclaration(t, i, p, _) ->
            if self.ContainsKey i then
                raise (functionAlreadyDefined i)
            self.Add(i, { ReturnType = t; ParameterTypes = List.map typeOfDeclaration p; })

    do
        // First add built-in methods
        self.Add("iread",  { ReturnType = Int; ParameterTypes = []; })
        self.Add("iprint", { ReturnType = Void; ParameterTypes = [ { Type = Int; IsArray = false } ]; })
        self.Add("fread",  { ReturnType = Float; ParameterTypes = []; })
        self.Add("fprint", { ReturnType = Void; ParameterTypes = [ { Type = Float; IsArray = false } ]; })
        program |> List.iter scanDeclaration

type SymbolTable(program) as self =
    inherit Dictionary<IdentifierRef, VariableDeclaration>(HashIdentity.Reference)

    let whileStatementStack = Stack<WhileStatement>()
    let symbolScopeStack = new SymbolScopeStack()

    let rec scanDeclaration =
        function
        | StaticVariableDeclaration(x) -> symbolScopeStack.AddDeclaration x
        | FunctionDeclaration(x)       -> scanFunctionDeclaration x

    and scanFunctionDeclaration (functionReturnType, _, parameters, compoundStatement) =
        let rec scanCompoundStatement (localDeclarations, statements) =
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
                whileStatementStack.Push (e, s)
                scanExpression e
                scanStatement s
                whileStatementStack.Pop() |> ignore
            | ReturnStatement(Some(e)) ->
                scanExpression e
            | ReturnStatement(None) ->
                if functionReturnType <> Void then
                    raise (cannotConvertType (Void.ToString()) (functionReturnType.ToString()))
            | BreakStatement ->
                if whileStatementStack.Count = 0 then
                    raise (noEnclosingLoop())

        and addIdentifierMapping identifierRef =
            let declaration = symbolScopeStack.CurrentScope.FindDeclaration identifierRef
            self.Add(identifierRef, declaration)

        and scanExpression =
            function
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

        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration
        scanCompoundStatement compoundStatement
        symbolScopeStack.Pop() |> ignore

    do program |> List.iter scanDeclaration

    member x.GetIdentifierTypeSpec identifierRef =
        typeOfDeclaration self.[identifierRef]

type ExpressionTypeDictionary(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Expression, VariableType>(HashIdentity.Reference)

    let rec scanDeclaration =
        function
        | FunctionDeclaration(x) -> scanFunctionDeclaration x
        | _ -> ()

    and scanFunctionDeclaration (functionReturnType, _, _, compoundStatement) =
        let rec scanCompoundStatement (_, statements) =
            statements |> List.iter scanStatement

        and scanStatement =
            function
            | ExpressionStatement(es) ->
                match es with
                | Expression(e) -> scanExpression e |> ignore
                | Nop -> ()
            | CompoundStatement(x) -> scanCompoundStatement x
            | IfStatement(e, s1, Some(s2)) ->
                scanExpression e |> ignore
                scanStatement s1
                scanStatement s2
            | IfStatement(e, s1, None) ->
                scanExpression e |> ignore
                scanStatement s1
            | WhileStatement(e, s) ->
                scanExpression e |> ignore
                scanStatement s
            | ReturnStatement(Some(e)) ->
                let typeOfE = scanExpression e
                if typeOfE <> scalarType functionReturnType then raise (cannotConvertType (typeOfE.ToString()) (functionReturnType.ToString()))
            | _ -> ()

        and scanExpression expression =
            let checkArrayIndexType e =
                let arrayIndexType = scanExpression e
                if arrayIndexType <> scalarType Int then
                    raise (cannotConvertType (arrayIndexType.ToString()) (Int.ToString()))

            let expressionType =
                match expression with
                | AssignmentExpression(ae) as x ->
                    match ae with
                    | ScalarAssignmentExpression(i, e) ->
                        let typeOfE = scanExpression e
                        let typeOfI = symbolTable.GetIdentifierTypeSpec i
                        if typeOfE <> typeOfI then raise (cannotConvertType (typeOfE.ToString()) (typeOfI.ToString()))
                        typeOfI
                    | ArrayAssignmentExpression(i, e1, e2) ->
                        checkArrayIndexType e1

                        let typeOfE2 = scanExpression e2
                        let typeOfI = symbolTable.GetIdentifierTypeSpec i

                        if not typeOfI.IsArray then
                            raise (cannotApplyIndexing (typeOfI.ToString()))

                        if typeOfE2.IsArray then
                            raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))

                        if typeOfE2.Type <> typeOfI.Type then raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))

                        scalarType typeOfI.Type
                | BinaryExpression(e1, op, e2) ->
                    let typeOfE1 = scanExpression e1
                    let typeOfE2 = scanExpression e2
                    match op with
                    | ConditionalOr | ConditionalAnd ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Bool; IsArray = false; }, { Type = Bool; IsArray = false; } -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | Equal | NotEqual ->
                        match typeOfE1, typeOfE2 with
                        | { Type = a; IsArray = false; }, { Type = b; IsArray = false; } when a = b && a <> Void -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | LessEqual | Less | GreaterEqual | Greater ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Int; IsArray = false; }, { Type = Int; IsArray = false; }
                        | { Type = Float; IsArray = false; }, { Type = Float; IsArray = false; } ->
                            ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | Add | Subtract | Multiply | Divide | Modulus ->
                        typeOfE1
                | UnaryExpression(_, e) ->
                    scanExpression e
                | IdentifierExpression(i) ->
                    symbolTable.GetIdentifierTypeSpec i
                | ArrayIdentifierExpression(i, e) ->
                    checkArrayIndexType e
                    scalarType (symbolTable.GetIdentifierTypeSpec i).Type
                | FunctionCallExpression(i, a) ->
                    if not (functionTable.ContainsKey i) then
                        raise (nameDoesNotExist i)
                    let calledFunction = functionTable.[i]
                    let parameterTypes = calledFunction.ParameterTypes
                    if List.length a <> List.length parameterTypes then
                        raise (wrongNumberOfArguments i (List.length parameterTypes) (List.length a))
                    let argumentTypes = a |> List.map scanExpression
                    let checkTypesMatch index l r =
                        if l <> r then raise (invalidArguments i (index + 1) (l.ToString()) (r.ToString()))
                    List.iteri2 checkTypesMatch argumentTypes parameterTypes
                    scalarType calledFunction.ReturnType
                | ArraySizeExpression(i) ->
                    scalarType Int
                | LiteralExpression(l) ->
                    match l with
                    | BoolLiteral(b)  -> scalarType Bool
                    | IntLiteral(i)   -> scalarType Int
                    | FloatLiteral(f) -> scalarType Float
                | ArrayAllocationExpression(t, e) ->
                    checkArrayIndexType e
                    { Type = t; IsArray = true }

            self.Add(expression, expressionType)

            expressionType

        scanCompoundStatement compoundStatement

    do program |> List.iter scanDeclaration

type SemanticAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeDictionary;
    }

let analyze program =
    let symbolTable   = new SymbolTable(program)
    let functionTable = new FunctionTable(program)

    if not (functionTable.ContainsKey "main") then
        raise (missingEntryPoint())

    let expressionTypes = new ExpressionTypeDictionary(program, functionTable, symbolTable)

    {
        SymbolTable     = symbolTable;
        ExpressionTypes = expressionTypes;
    }