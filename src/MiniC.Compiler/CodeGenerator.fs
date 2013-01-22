module MiniC.Compiler.CodeGenerator

open System.Reflection
open System.Reflection.Emit
open IL

let emitOpCode (ilGenerator : ILGenerator) = function
    | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
    | Ret       -> ilGenerator.Emit(OpCodes.Ret)

let generateMethod (typeBuilder : TypeBuilder) (ilMethod : ILMethod) =
    let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
    let methodBuilder = typeBuilder.DefineMethod(ilMethod.Name, methodAttributes)

    methodBuilder.SetReturnType ilMethod.ReturnType
    methodBuilder.SetParameters (List.toArray (ilMethod.Parameters |> List.map (fun p -> p.Type)))

    let ilGenerator = methodBuilder.GetILGenerator()
    ilMethod.Body |> List.iter (emitOpCode ilGenerator)

let generateField (typeBuilder : TypeBuilder) (ilField : ILField) =
    let fieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
    typeBuilder.DefineField(ilField.Name, ilField.Type, fieldAttributes) |> ignore

let generateType (moduleBuilder : ModuleBuilder) (ilClass : ILClass) moduleName =
    let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
    let typeBuilder = moduleBuilder.DefineType(moduleName + ".Program", typeAttributes)

    ilClass.Fields  |> List.iter (generateField  typeBuilder)
    ilClass.Methods |> List.iter (generateMethod typeBuilder)