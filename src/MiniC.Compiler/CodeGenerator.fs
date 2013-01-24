namespace MiniC.Compiler

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open IL

type CodeGenerator(moduleBuilder : ModuleBuilder, ilClass : ILClass, moduleName : string) =
    let fieldMappings = new Dictionary<ILVariable, FieldInfo>()

    let emitOpCode (ilGenerator : ILGenerator) = function
        | Add       -> ilGenerator.Emit(OpCodes.Add)
        | Ldarg(i)  -> ilGenerator.Emit(OpCodes.Ldarg, i)
        | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | Ldloc(i)  -> ilGenerator.Emit(OpCodes.Ldloc, i)
        | Ldfld(v)  -> ilGenerator.Emit(OpCodes.Ldfld, fieldMappings.[v])
        | Mul       -> ilGenerator.Emit(OpCodes.Mul)
        | Ret       -> ilGenerator.Emit(OpCodes.Ret)
        | Starg(i)  -> ilGenerator.Emit(OpCodes.Starg, i)
        | Stfld(v)  -> ilGenerator.Emit(OpCodes.Stfld, fieldMappings.[v])
        | Stloc(i)  -> ilGenerator.Emit(OpCodes.Stloc, i)
        | Sub       -> ilGenerator.Emit(OpCodes.Sub)

    let emitLocal (ilGenerator : ILGenerator) variable =
        ilGenerator.DeclareLocal(variable.Type) |> ignore //.SetLocalSymInfo(variable.Name)

    let generateMethod (typeBuilder : TypeBuilder) (ilMethod : ILMethod) =
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        let methodBuilder = typeBuilder.DefineMethod(ilMethod.Name, methodAttributes)

        methodBuilder.SetReturnType ilMethod.ReturnType
        methodBuilder.SetParameters (List.toArray (ilMethod.Parameters |> List.map (fun p -> p.Type)))

        let ilGenerator = methodBuilder.GetILGenerator()
        ilMethod.Locals |> List.iter (emitLocal ilGenerator)
        ilMethod.Body |> List.iter (emitOpCode ilGenerator)

    let generateField (typeBuilder : TypeBuilder) (ilField : ILVariable) =
        let fieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
        let fieldBuilder = typeBuilder.DefineField(ilField.Name, ilField.Type, fieldAttributes)
        fieldMappings.Add(ilField, fieldBuilder)

    member x.GenerateType() =
        let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
        let typeBuilder = moduleBuilder.DefineType(moduleName + ".Program", typeAttributes)

        ilClass.Fields  |> List.iter (generateField  typeBuilder)
        ilClass.Methods |> List.iter (generateMethod typeBuilder)

        (typeBuilder.CreateType(), typeBuilder.GetMethod("main")) // TODO