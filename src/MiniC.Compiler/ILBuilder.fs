module MiniC.Compiler.ILBuilder

open IL

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
    | _ -> failwith "Not implemented"

and processLiteralExpression =
    function
    | Ast.IntLiteral(x) -> [ Ldc_I4(x) ]
    | _ -> failwith "Not implemented"

and processExpression =
    function
    | Ast.AssignmentExpression(x) -> processAssignmentExpression x
    | Ast.BinaryExpression(a, b, c) -> processBinaryExpression (a, b, c)
    | Ast.LiteralExpression(x) -> processLiteralExpression x
    | _ -> failwith "Not implemented"

and processAssignmentExpression =
    function
//    | Ast.ScalarAssignmentExpression(i, e) -> List.concat [ (processExpression e); [ Ldfld(new ILVariable()) ] ]
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
    | Ast.ScalarLocalDeclaration(t, i) ->
        {
            ILVariable.Type = getType t; 
            Name = i;
        }
    | _ -> failwith "Not implemented"

let processFunctionDeclaration (returnType, name, parameters, (localDeclarations, statements)) =
    {
        Name       = name;
        ReturnType = getType returnType;
        Parameters = localDeclarations |> List.map processLocalDeclaration;
        Locals     = [];
        Body       = statements |> List.collect processStatement;
    }

let processDeclaration = 
    function
    | Ast.FunctionDeclaration(x) -> processFunctionDeclaration x
    | Ast.VariableDeclaration(x) -> failwith "Not implemented"

let buildClass (program : Ast.Program) =
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