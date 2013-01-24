namespace MiniC.Compiler

open System.Collections.Generic
open IL

type ILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type ILBuilder(symbolEnvironment) =
    let variableMappings = new Dictionary<Ast.IVariableDeclaration, ILVariableScope>(HashIdentity.Reference)
    let mutable argumentIndex = 0s // TODO: Won't work for multiple function declarations
    let mutable localIndex = 0s // TODO: Won't work for multiple compound statements

    let lookupILVariableScope expression =
        let declaration = SymbolEnvironment.findDeclaration expression symbolEnvironment
        variableMappings.[declaration]

    let getType =
        function
        | Ast.Void  -> typeof<unit>
        | Ast.Bool  -> typeof<bool>
        | Ast.Int   -> typeof<int>
        | Ast.Float -> typeof<float>

    let getParameterType =
        function
        | Ast.ScalarParameter(typeSpec, _) -> getType typeSpec
        | Ast.ArrayParameter(typeSpec, _)  -> failwith "Not implemented"

    let rec processBinaryExpression =
        function
        | l, Ast.Add, r -> List.concat [ (processExpression l); (processExpression r); [ Add ] ]
        | l, Ast.Multiply, r -> List.concat [ (processExpression l); (processExpression r); [ Mul ] ]
        | l, Ast.Subtract, r -> List.concat [ (processExpression l); (processExpression r); [ Sub ] ]
        | _ -> failwith "Not implemented"

    and processLiteralExpression =
        function
        | Ast.IntLiteral(x) -> [ Ldc_I4(x) ]
        | _ -> failwith "Not implemented"

    and processIdentifierLoad expression =
        match lookupILVariableScope expression with
        | FieldScope(v)    -> [ Ldfld v ]
        | ArgumentScope(i) -> [ Ldarg i ]
        | LocalScope(i)    -> [ Ldloc i ]

    and processIdentifierStore expression =
        match lookupILVariableScope expression with
        | FieldScope(v)    -> [ Stfld v ]
        | ArgumentScope(i) -> [ Starg i ]
        | LocalScope(i)    -> [ Stloc i ]

    and processIdentifierExpression expression identifier =
        processIdentifierLoad expression

    and processExpression expression =
        match expression with
        | Ast.AssignmentExpression(x) -> processAssignmentExpression expression x
        | Ast.BinaryExpression(a, b, c) -> processBinaryExpression (a, b, c)
        | Ast.LiteralExpression(x) -> processLiteralExpression x
        | Ast.IdentifierExpression(i) -> processIdentifierExpression expression i
        | _ -> failwith "Not implemented"

    and processAssignmentExpression expression =
        function
        | Ast.ScalarAssignmentExpression(i, e) as ae ->
            List.concat [ (processExpression e); processIdentifierStore expression ]
        | _ -> failwith "Not implemented"

    and processReturnStatement =
        function
        | Some(x) -> (processExpression x) @ [ Ret ]
        | None    -> [ Ret ]

    and processStatement =
        function
        | Ast.ExpressionStatement(x) -> processExpressionStatement x
        | Ast.CompoundStatement(_, s) -> s |> List.collect processStatement
        | Ast.ReturnStatement(x) -> processReturnStatement x
        | _ -> failwith "Not implemented"

    and processExpressionStatement =
        function
        | Ast.Expression(x) -> processExpression x
        | Ast.Nop -> []

    let processLocalDeclaration =
        function
        | Ast.ScalarLocalDeclaration(t, i) as d ->
            let v = {
                ILVariable.Type = getType t; 
                Name = i;
            }
            variableMappings.Add(d, LocalScope localIndex)
            localIndex <- localIndex + 1s
            v
        | _ -> failwith "Not implemented"

    let processParameter =
        function
        | Ast.ScalarParameter(t, i) as d ->
            let v = {
                ILVariable.Type = getType t; 
                Name = i;
            }
            variableMappings.Add(d, ArgumentScope argumentIndex)
            argumentIndex <- argumentIndex + 1s
            v
        | _ -> failwith "Not implemented"

    let processFunctionDeclaration (returnType, name, parameters, (localDeclarations, statements)) =
        {
            Name       = name;
            ReturnType = getType returnType;
            Parameters = parameters |> List.map processParameter;
            Locals     = localDeclarations |> List.map processLocalDeclaration;
            Body       = statements |> List.collect processStatement;
        }

    let processDeclaration = 
        function
        | Ast.FunctionDeclaration(x) -> processFunctionDeclaration x
        | Ast.VariableDeclaration(x) -> failwith "Not implemented"

    member x.BuildClass (program : Ast.Program) =
        let functionDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | Ast.FunctionDeclaration(_, _, _, _ as a) -> Some a
                | _ -> None)
    
        {
            Fields  = [];
            Methods = functionDeclarations |> List.map processFunctionDeclaration;
        }