module MiniC.Compiler.AstUtilities

open Ast

let typeOf =
    function
    | Void  -> typeof<System.Void>
    | Bool  -> typeof<bool>
    | Int   -> typeof<int>
    | Float -> typeof<float>