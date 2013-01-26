module MiniC.Compiler.AstUtilities

open Ast

let typeOf =
    function
    | Void  -> typeof<System.Void>
    | Bool  -> typeof<bool>
    | Int   -> typeof<int>
    | Float -> typeof<float>

let typeOfVariableDeclaration =
    function
    | ScalarVariableDeclaration(typeSpec, _)
    | ArrayVariableDeclaration(typeSpec, _)  -> typeOf typeSpec

//let typeofExpression symbolEnvironment =
//    function
//    | Ast.AssignmentExpression(x) ->
//        match x with
//        | Ast.ScalarAssignmentExpression(i, e) -> 
//            let declaration = SymbolEnvironment.findDeclaration e symbolEnvironment
            