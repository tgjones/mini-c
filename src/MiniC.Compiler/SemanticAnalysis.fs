module MiniC.Compiler.SemanticAnalysis

open System.Collections.Generic
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
            raise (CompilerException(sprintf "CS003 A variable named '%s' is already defined in this scope" (identifierFromDeclaration declaration)))
        list <- declaration :: list

    member x.FindDeclaration identifierRef =
        let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list
        match found with
        | Some(d) -> d
        | None ->
            match parent with
            | Some(ss) -> ss.FindDeclaration identifierRef
            | None -> raise (CompilerException(sprintf "CS006 The name '%s' does not exist in the current context" (identifierRef.Identifier)))

type private SymbolScopeStack() =
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))

    member x.CurrentScope = stack.Peek()

    member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member x.Pop() = stack.Pop() |> ignore
    member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration

type VariableType = TypeSpec * bool

type FunctionTableEntry =
    {
        ReturnType     : TypeSpec;
        ParameterTypes : TypeSpec list;
    }

let typeOfDeclaration =
    function
    | Ast.ScalarVariableDeclaration(t, _)
    | Ast.ArrayVariableDeclaration(t, _) ->
        t

type FunctionTable(program) as self =
    inherit Dictionary<Identifier, FunctionTableEntry>()

    let rec scanDeclaration =
        function
        | StaticVariableDeclaration(x)    -> ()
        | FunctionDeclaration(t, i, p, _) ->
            self.Add(i, { ReturnType = t; ParameterTypes = List.map typeOfDeclaration p; })

    do
        // First add built-in methods
        self.Add("iread",  { ReturnType = Int; ParameterTypes = []; })
        self.Add("iprint", { ReturnType = Void; ParameterTypes = [ Int ]; })
        self.Add("fread",  { ReturnType = Float; ParameterTypes = []; })
        self.Add("fprint", { ReturnType = Void; ParameterTypes = [ Float ]; })
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
                    raise (CompilerException (sprintf "CS004 Cannot convert type '%s' to '%s'" (Void.ToString()) (functionReturnType.ToString())))
            | BreakStatement ->
                if whileStatementStack.Count = 0 then
                    raise (CompilerException "CS009 No enclosing loop out of which to break")

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
        let declaration = self.[identifierRef]
        match declaration with
        | ScalarVariableDeclaration(t, _)
        | ArrayVariableDeclaration(t, _)  -> t

type ExpressionTypeDictionary(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Expression, TypeSpec>(HashIdentity.Reference)

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
                if typeOfE <> functionReturnType then raise (CompilerException (sprintf "CS004 Cannot convert type '%s' to '%s'" (typeOfE.ToString()) (functionReturnType.ToString())))
            | _ -> ()

        and scanExpression expression =
            let checkArrayIndexType e =
                let arrayIndexType = scanExpression e
                if arrayIndexType <> Int then
                    raise (CompilerException (sprintf "CS004 Cannot convert type '%s' to '%s'" (arrayIndexType.ToString()) (Int.ToString())))

            let expressionType =
                match expression with
                | AssignmentExpression(ae) as x ->
                    match ae with
                    | ScalarAssignmentExpression(i, e) ->
                        let typeOfE = scanExpression e
                        let typeOfI = symbolTable.GetIdentifierTypeSpec i
                        if typeOfE <> typeOfI then raise (CompilerException (sprintf "CS004 Cannot convert type '%s' to '%s'" (typeOfE.ToString()) (typeOfI.ToString())))
                        typeOfI
                    | ArrayAssignmentExpression(i, e1, e2) ->
                        checkArrayIndexType e1

                        let typeOfE2 = scanExpression e2
                        let typeOfI = symbolTable.GetIdentifierTypeSpec i
                        if typeOfE2 <> typeOfI then raise (CompilerException (sprintf "CS004 Cannot convert type '%s' to '%s'" (typeOfE2.ToString()) (typeOfI.ToString())))

                        typeOfI
                | BinaryExpression(e1, op, e2) ->
                    let typeOfE1 = scanExpression e1
                    let typeOfE2 = scanExpression e2
                    match op with
                    | ConditionalOr | ConditionalAnd ->
                        match typeOfE1, typeOfE2 with
                        | Bool, Bool -> ()
                        | _ -> raise (CompilerException (sprintf "CS005 Operator '%s' cannot be applied to operands of type '%s' and '%s'" (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString())))
                        Bool
                    | Equal | NotEqual ->
                        match typeOfE1, typeOfE2 with
                        | Int, Int
                        | Float, Float
                        | Bool, Bool ->
                            ()
                        | _ -> raise (CompilerException (sprintf "CS005 Operator '%s' cannot be applied to operands of type '%s' and '%s'" (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString())))
                        Bool
                    | LessEqual | Less | GreaterEqual | Greater ->
                        match typeOfE1, typeOfE2 with
                        | Int, Int
                        | Float, Float ->
                            ()
                        | _ -> raise (CompilerException (sprintf "CS005 Operator '%s' cannot be applied to operands of type '%s' and '%s'" (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString())))
                        Bool
                    | Add | Subtract | Multiply | Divide | Modulus ->
                        typeOfE1 // TODO: Widen int to float
                | UnaryExpression(_, e) ->
                    scanExpression e
                | IdentifierExpression(i) ->
                    symbolTable.GetIdentifierTypeSpec i
                | ArrayIdentifierExpression(i, e) ->
                    checkArrayIndexType e
                    symbolTable.GetIdentifierTypeSpec i
                | FunctionCallExpression(i, a) ->
                    if not (functionTable.ContainsKey i) then
                        raise (CompilerException(sprintf "CS006 The name '%s' does not exist in the current context" i))
                    let calledFunction = functionTable.[i]
                    let parameterTypes = calledFunction.ParameterTypes
                    if List.length a <> List.length parameterTypes then
                        raise (CompilerException(sprintf "CS008 Function '%s' takes %i arguments, but here was given %i" i (List.length parameterTypes) (List.length a)))
                    let argumentTypes = a |> List.map scanExpression
                    let checkTypesMatch index l r =
                        if l <> r then raise (CompilerException (sprintf "CS007 Call to function '%s' has some invalid arguments. Argument %i: Cannot convert from '%s' to '%s'" i (index + 1) (l.ToString()) (r.ToString())))
                    List.iteri2 checkTypesMatch argumentTypes parameterTypes
                    calledFunction.ReturnType
                | ArraySizeExpression(i) ->
                    Ast.Int
                | LiteralExpression(l) ->
                    match l with
                    | BoolLiteral(b)  -> Ast.Bool
                    | IntLiteral(i)   -> Ast.Int
                    | FloatLiteral(f) -> Ast.Float
                | ArrayAllocationExpression(t, e) ->
                    checkArrayIndexType e
                    t

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
    let expressionTypes = new ExpressionTypeDictionary(program, functionTable, symbolTable)

    {
        SymbolTable     = symbolTable;
        ExpressionTypes = expressionTypes;
    }