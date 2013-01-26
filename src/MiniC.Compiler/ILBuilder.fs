namespace MiniC.Compiler

open System.Collections.Generic
open AstUtilities
open SemanticAnalysis
open IL

type private ILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type private VariableMappingDictionary = Dictionary<Ast.VariableDeclaration, ILVariableScope>

type ILMethodBuilder(semanticAnalysisResult : SemanticAnalysisResult,
                     variableMappings : VariableMappingDictionary) =
    let mutable argumentIndex = 0s
    let mutable localIndex = 0s
    let mutable labelIndex = 0
    let mutable currentWhileStatementEndLabel = ILLabel()

    let lookupILVariableScope identifierRef =
        let declaration = semanticAnalysisResult.SymbolTable.[identifierRef]
        variableMappings.[declaration]

    let makeLabel() =
        let result = labelIndex
        labelIndex <- labelIndex + 1
        result

    let rec processBinaryExpression =
        function
        | (l, Ast.ConditionalOr, r) ->
            let leftIsFalseLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression l
                          [ Brfalse leftIsFalseLabel ]
                          [ Ldc_I4 1 ]
                          [ Br endLabel ]
                          [ Label leftIsFalseLabel ]
                          processExpression r
                          [ Label endLabel ] ]
        | (l, Ast.ConditionalAnd, r) ->
            let leftIsTrueLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression l
                          [ Brtrue leftIsTrueLabel ]
                          [ Ldc_I4 0 ]
                          [ Br endLabel ]
                          [ Label leftIsTrueLabel ]
                          processExpression r
                          [ Label endLabel ] ]
        | (l, op, r) -> List.concat [ (processExpression l);
                                      (processExpression r);
                                      [ processBinaryOperator op ] ]

    and processBinaryOperator =
        function
        | Ast.Add -> Add
        | Ast.Divide -> Div
        | Ast.Multiply -> Mul
        | Ast.Modulus -> Rem
        | Ast.Subtract -> Sub
        | Ast.Equal -> Ceq
        | Ast.Greater -> Cgt
        | Ast.GreaterEqual -> Cge
        | Ast.Less -> Clt
        | Ast.LessEqual -> Cle
        | _ -> failwith "Shouldn't be here"

    and processLiteralExpression =
        function
        | Ast.IntLiteral(x)   -> [ Ldc_I4(x) ]
        | Ast.FloatLiteral(x) -> [ Ldc_R8(x) ]
        | Ast.BoolLiteral(x)  -> [ (if x then Ldc_I4(1) else Ldc_I4(0)) ]

    and processIdentifierLoad identifierRef =
        match lookupILVariableScope identifierRef with
        | FieldScope(v)    -> [ Ldsfld v ]
        | ArgumentScope(i) -> [ Ldarg i ]
        | LocalScope(i)    -> [ Ldloc i ]

    and processIdentifierStore identifierRef =
        match lookupILVariableScope identifierRef with
        | FieldScope(v)    -> [ Stsfld v ]
        | ArgumentScope(i) -> [ Starg i ]
        | LocalScope(i)    -> [ Stloc i ]

    and processIdentifierExpression identifierRef =
        processIdentifierLoad identifierRef

    and processExpression expression =
        match expression with
        | Ast.AssignmentExpression(x) -> processAssignmentExpression expression x
        | Ast.BinaryExpression(a, b, c) -> processBinaryExpression (a, b, c)
        | Ast.UnaryExpression(a, b) -> processUnaryExpression (a, b)
        | Ast.LiteralExpression(x) -> processLiteralExpression x
        | Ast.IdentifierExpression(i) -> processIdentifierExpression i
        | Ast.FunctionCallExpression(i, a) -> processFunctionCallExpression (i, a)
        | _ -> failwith "Not implemented"

    and processAssignmentExpression expression =
        function
        | Ast.ScalarAssignmentExpression(i, e) as ae ->
            List.concat [ processExpression e
                          [ Dup ]
                          processIdentifierStore i ]
        | _ -> failwith "Not implemented"

    and processUnaryExpression (operator, expression) =
        List.concat [ processExpression expression
                      processUnaryOperator operator ]

    and processUnaryOperator =
        function
        | Ast.LogicalNegate -> [ Ldc_I4(0); Ceq ]
        | Ast.Negate        -> [ Neg ]
        | Ast.Identity      -> [ ]

    and processFunctionCallExpression (identifier, arguments) =
        List.concat [ arguments |> List.collect processExpression
                      [ Call(identifier) ] ]

    and processReturnStatement =
        function
        | Some(x) -> (processExpression x) @ [ Ret ]
        | None    -> [ Ret ]

    and processStatement =
        function
        | Ast.ExpressionStatement(x) -> processExpressionStatement x
        | Ast.CompoundStatement(_, s) -> s |> List.collect processStatement
        | Ast.IfStatement(e, s1, Some(s2)) ->
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ Brtrue thenLabel ]
                          processStatement s2
                          [ Br endLabel ]
                          [ Label thenLabel ]
                          processStatement s1
                          [ Label endLabel ] ]
        | Ast.IfStatement(e, s1, None) ->
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ Brtrue thenLabel ]
                          [ Br endLabel ]
                          [ Label thenLabel ]
                          processStatement s1
                          [ Label endLabel ] ]
        | Ast.WhileStatement(e, s) ->
            let startLabel = makeLabel()
            let conditionLabel = makeLabel()
            let endLabel = makeLabel()
            currentWhileStatementEndLabel <- endLabel
            List.concat [ [ Br conditionLabel ]
                          [ Label startLabel ]
                          processStatement s
                          [ Label conditionLabel ]
                          processExpression e
                          [ Brtrue startLabel ]
                          [ Label endLabel ] ]
        | Ast.ReturnStatement(x) -> processReturnStatement x
        | Ast.BreakStatement -> [ Br currentWhileStatementEndLabel ]

    and processExpressionStatement =
        function
        | Ast.Expression(x) ->
            processExpression x
        | Ast.Nop -> []

    let processVariableDeclaration (mutableIndex : byref<_>) f =
        function
        | Ast.ScalarVariableDeclaration(t, i) as d ->
            let v = {
                ILVariable.Type = typeOf t; 
                Name = i;
            }
            let i = mutableIndex
            variableMappings.Add(d, f i)
            mutableIndex <- mutableIndex + 1s
            v
        | _ -> failwith "Not implemented"

    let processLocalDeclaration declaration =
        processVariableDeclaration &localIndex (fun i -> LocalScope i) declaration
    let processParameter declaration =
        processVariableDeclaration &argumentIndex (fun i -> ArgumentScope i) declaration

    let rec collectLocalDeclarations =
        function
        | Ast.CompoundStatement(localDeclarations, statements) ->
             List.concat [ localDeclarations |> List.map processLocalDeclaration;
                           statements |> List.collect collectLocalDeclarations ]
        | _ -> []

    member x.BuildMethod(returnType, name, parameters, (localDeclarations, statements)) =
        {
            Name       = name;
            ReturnType = typeOf returnType;
            Parameters = parameters |> List.map processParameter;
            Locals     = List.concat [ localDeclarations |> List.map processLocalDeclaration;
                                       statements |> List.collect collectLocalDeclarations ]
            Body       = statements |> List.collect processStatement;
        }

type ILBuilder(semanticAnalysisResult) =
    let variableMappings = new VariableMappingDictionary(HashIdentity.Reference)

    let processVariableDeclaration =
        function
        | Ast.ScalarVariableDeclaration(t, i) as d -> 
            let v = {
                ILVariable.Type = typeOf t; 
                Name = i;
            }
            variableMappings.Add(d, FieldScope(v))
            v
        | Ast.ArrayVariableDeclaration(t, i) -> failwith "Not implemented"

    member x.BuildClass (program : Ast.Program) =
        let variableDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | Ast.StaticVariableDeclaration(x) -> Some(x)
                | _ -> None)
    
        let functionDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | Ast.FunctionDeclaration(_, _, _, _ as a) -> Some a
                | _ -> None)

        let processFunctionDeclaration functionDeclaration =
            let ilMethodBuilder = new ILMethodBuilder(semanticAnalysisResult, variableMappings)
            ilMethodBuilder.BuildMethod functionDeclaration

        {
            Fields  = variableDeclarations |> List.map processVariableDeclaration;
            Methods = functionDeclarations |> List.map processFunctionDeclaration;
        }