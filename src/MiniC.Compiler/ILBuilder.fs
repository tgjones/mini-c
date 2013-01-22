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

let processFunctionDeclaration (returnType, name, parameters, (localDeclarations, statements)) =
    {
        Name       = name;
        ReturnType = getType returnType;
        Parameters = [];
        Body       = [ Ret ];
    }

let processDeclaration = 
    function
    | Ast.FunctionDeclaration(x, y, z, w) -> processFunctionDeclaration (x, y, z, w)
    | Ast.VariableDeclaration(x) -> failwith "Not implemented"

//let buildClass (program : Ast.Program) =
//    let functionDeclarations =
//        program
//        |> List.filter (fun x ->
//            match x with
//            | Ast.FunctionDeclaration(_, _, _, _) as a -> a
//            | _ -> false)
//    
//    {
//        Fields  = [];
//        Methods = functionDeclarations |> List.map processFunctionDeclaration;
//    }