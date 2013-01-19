module MiniC.Compiler.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Ast

let getType =
    function
    | Void  -> typeof<Void>
    | Bool  -> typeof<bool>
    | Int   -> typeof<int>
    | Float -> typeof<float>

let getParameterType =
    function
    | ScalarParameter(typeSpec, identifier) -> getType typeSpec
    | ArrayParameter(typeSpec, identifier) -> failwith "Not implemented"

type CodeGenerator(assemblyBuilder : AssemblyBuilder, typeBuilder : TypeBuilder) =
    let generateFunctionDeclaration (returnType, name, parameters, (localDeclarations, statements)) =
        let methodBuilder =
            typeBuilder.DefineMethod(name,
                MethodAttributes.Static ||| MethodAttributes.Public)
        
        methodBuilder.SetReturnType (getType returnType)
        
        let parameterTypes = List.map getParameterType parameters
        methodBuilder.SetParameters (List.toArray parameterTypes)

        let ilGenerator = methodBuilder.GetILGenerator()

        let rec emitStatementList =
            function
            | statement :: otherStatements ->
                emitStatement statement
                emitStatementList otherStatements
            | [] -> ()
             
        and emitStatement =
            function
            | CompoundStatement(localDeclarations, statements) -> emitStatementList statements
            | ExpressionStatement(x) -> ()
            | ReturnStatement(optionalExpression) ->
                match optionalExpression with
                | Some(expression) -> emitExpression expression
                | None -> ()

                ilGenerator.Emit(OpCodes.Ret)

        and emitExpression =
            function
            | LiteralExpression(literal) -> emitLiteral literal
            | _ -> failwith "Not implemented"

        and emitLiteral =
            function
            | IntLiteral(int) -> ilGenerator.Emit(OpCodes.Ldc_I4, int)
            | _ -> failwith "Not implemented"

        emitStatementList statements

        if name = "main" then
            assemblyBuilder.SetEntryPoint methodBuilder

    let generateDeclaration = 
        function
        | FunctionDeclaration(x, y, z, w) -> generateFunctionDeclaration (x, y, z, w)
        | VariableDeclaration -> ()

    member x.Generate (program : Program) =
        List.iter generateDeclaration program

let compileToMemory assemblyName code =
    let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
    let m = a.DefineDynamicModule (assemblyName.Name, assemblyName.Name + ".exe")
    let typeBuilder = m.DefineType(assemblyName.Name + ".Program", TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public)
    
    let codeGenerator = new CodeGenerator(a, typeBuilder)
    codeGenerator.Generate(code)

    let compiledType = typeBuilder.CreateType()
    (compiledType, compiledType.GetMethod("main")) // TODO