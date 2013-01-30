namespace MiniC.Compiler

open System.Collections.Generic
open SemanticAnalysis
open IL

type private ILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type private VariableMappingDictionary = Dictionary<Ast.VariableDeclaration, ILVariableScope>

module private ILBuilderUtilities =
    let typeOf =
        function
        | Ast.Void  -> typeof<System.Void>
        | Ast.Bool  -> typeof<bool>
        | Ast.Int   -> typeof<int>
        | Ast.Float -> typeof<float>

    let createILVariable =
        function
        | Ast.ScalarVariableDeclaration(t, i) as d ->
            {
                ILVariable.Type = typeOf t; 
                Name = i;
            }
        | Ast.ArrayVariableDeclaration(t, i) as d ->
            {
                ILVariable.Type = (typeOf t).MakeArrayType(); 
                Name = i;
            }

open ILBuilderUtilities

type ILMethodBuilder(semanticAnalysisResult : SemanticAnalysisResult,
                     variableMappings : VariableMappingDictionary) =
    let mutable argumentIndex = 0s
    let mutable localIndex = 0s
    let arrayAssignmentLocals = Dictionary<Ast.Expression, int16>()
    let mutable labelIndex = 0
    let currentWhileStatementEndLabel = Stack<ILLabel>()

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

    and processExpression expression =
        match expression with
        | Ast.ScalarAssignmentExpression(i, e) ->
            List.concat [ processExpression e
                          [ Dup ]
                          processIdentifierStore i ]
        | Ast.ArrayAssignmentExpression(i, e1, e2) as ae ->
            List.concat [ processIdentifierLoad i
                          processExpression e1
                          processExpression e2
                          [ Dup ]
                          [ Stloc arrayAssignmentLocals.[ae] ]
                          [ Stelem (typeOf (semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type) ]
                          [ Ldloc arrayAssignmentLocals.[ae] ] ]
        | Ast.BinaryExpression(a, b, c) -> processBinaryExpression (a, b, c)
        | Ast.UnaryExpression(op, e) ->
            List.concat [ processExpression e
                          processUnaryOperator op]
        | Ast.IdentifierExpression(i) -> processIdentifierLoad i
        | Ast.ArrayIdentifierExpression(i, e) ->
            List.concat [ processIdentifierLoad i
                          processExpression e
                          [ Ldelem (typeOf (semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type) ] ]
        | Ast.FunctionCallExpression(i, a) ->
            List.concat [ a |> List.collect processExpression
                          [ Call i ] ]
        | Ast.ArraySizeExpression(i) ->
            List.concat [ processIdentifierLoad i
                          [ Ldlen ] ]
        | Ast.LiteralExpression(l) ->
            match l with
            | Ast.IntLiteral(x)   -> [ Ldc_I4(x) ]
            | Ast.FloatLiteral(x) -> [ Ldc_R8(x) ]
            | Ast.BoolLiteral(x)  -> [ (if x then Ldc_I4(1) else Ldc_I4(0)) ]
        | Ast.ArrayAllocationExpression(t, e) ->
            List.concat [ processExpression e
                          [ Newarr (typeOf t) ] ]

    and processUnaryOperator =
        function
        | Ast.LogicalNegate -> [ Ldc_I4 0; Ceq ]
        | Ast.Negate        -> [ Neg ]
        | Ast.Identity      -> [ ]

    and processStatement =
        function
        | Ast.ExpressionStatement(x) ->
            match x with
            | Ast.Expression(x) ->
                let isNotVoid = semanticAnalysisResult.ExpressionTypes.[x].Type <> Ast.Void
                List.concat [ processExpression x
                              (if isNotVoid then [ Pop ] else []) ]
                
            | Ast.Nop -> []
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
            currentWhileStatementEndLabel.Push endLabel
            let result = List.concat [ [ Br conditionLabel ]
                                       [ Label startLabel ]
                                       processStatement s
                                       [ Label conditionLabel ]
                                       processExpression e
                                       [ Brtrue startLabel ]
                                       [ Label endLabel ] ]
            currentWhileStatementEndLabel.Pop() |> ignore
            result
        | Ast.ReturnStatement(x) ->
            match x with
            | Some(x) -> (processExpression x) @ [ Ret ]
            | None    -> [ Ret ]
        | Ast.BreakStatement ->
            [ Br (currentWhileStatementEndLabel.Peek()) ]

    let processVariableDeclaration (mutableIndex : byref<_>) f d =
        let v = createILVariable d
        variableMappings.Add(d, f mutableIndex)
        mutableIndex <- mutableIndex + 1s
        v

    let processLocalDeclaration declaration =
        processVariableDeclaration &localIndex (fun i -> LocalScope i) declaration
    let processParameter declaration =
        processVariableDeclaration &argumentIndex (fun i -> ArgumentScope i) declaration

    let rec collectLocalDeclarations statement =
        let rec fromStatement =
            function
            | Ast.ExpressionStatement(es) ->
                match es with
                | Ast.Expression(e) -> fromExpression e
                | Ast.Nop -> []
            | Ast.CompoundStatement(localDeclarations, statements) ->
                 List.concat [ localDeclarations |> List.map processLocalDeclaration;
                               statements |> List.collect collectLocalDeclarations ]
            | Ast.IfStatement(e, s1, Some(s2)) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1
                              collectLocalDeclarations s2 ]
            | Ast.IfStatement(e, s1, None) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1 ]
            | Ast.WhileStatement(e, s) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s ]
            | Ast.ReturnStatement(Some(e)) ->
                List.concat [ fromExpression e ]
            | _ -> []

        and fromExpression =
            function
            | Ast.ScalarAssignmentExpression(i, e) -> fromExpression e
            | Ast.ArrayAssignmentExpression(i, e1, e2) as ae ->
                let v = {
                    ILVariable.Type = typeOf ((semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type); 
                    Name = "ArrayAssignmentTemp" + string localIndex;
                }
                arrayAssignmentLocals.Add(ae, localIndex);
                localIndex <- localIndex + 1s
                List.concat [ [ v ]; fromExpression e2 ]
            | Ast.BinaryExpression(l, op, r)      -> List.concat [ fromExpression l; fromExpression r; ]
            | Ast.UnaryExpression(op, e)          -> fromExpression e
            | Ast.ArrayIdentifierExpression(i, e) -> fromExpression e
            | Ast.FunctionCallExpression(i, a)    -> a |> List.collect fromExpression
            | Ast.ArrayAllocationExpression(t, e) -> fromExpression e
            | _ -> []

        fromStatement statement

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

    let processStaticVariableDeclaration d =
        let v = createILVariable d
        variableMappings.Add(d, FieldScope(v))
        v

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

        let builtInMethods = [
            {
                Name = "iread";
                ReturnType = typeof<int>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToInt32", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "fread";
                ReturnType = typeof<float>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToDouble", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "iprint";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<int>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<int> |]))
                         Ret ];
            };
            {
                Name = "fprint";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<float>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<float> |]))
                         Ret ];
            } ]

        {
            Fields  = variableDeclarations |> List.map processStaticVariableDeclaration;
            Methods = List.concat [ builtInMethods
                                    functionDeclarations |> List.map processFunctionDeclaration ];
        }