namespace MiniC.Compiler

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open IL

type MethodMappingDictionary = Dictionary<string, MethodInfo>
type FieldMappingDictionary = Dictionary<ILVariable, FieldInfo>

type MethodGenerator(typeBuilder : TypeBuilder, ilMethod : ILMethod,
                     methodMappings : MethodMappingDictionary,
                     fieldMappings : FieldMappingDictionary) =
    let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
    let methodBuilder = typeBuilder.DefineMethod(ilMethod.Name, methodAttributes)
    do methodMappings.Add(ilMethod.Name, methodBuilder)

    let ilGenerator = methodBuilder.GetILGenerator()

    let labelMappings = new Dictionary<ILLabel, Label>()

    let getLabel ilLabel =
        if labelMappings.ContainsKey ilLabel then
            labelMappings.[ilLabel]
        else
            let label = ilGenerator.DefineLabel()
            labelMappings.Add(ilLabel, label)
            label

    let emitOpCode (ilGenerator : ILGenerator) = function
        | Add       -> ilGenerator.Emit(OpCodes.Add)
        | Br(l)     -> ilGenerator.Emit(OpCodes.Br, getLabel l)
        | Brtrue(l) -> ilGenerator.Emit(OpCodes.Brtrue, getLabel l)
        | Call(n)   -> ilGenerator.Emit(OpCodes.Call, methodMappings.[n])
        | Ceq       -> ilGenerator.Emit(OpCodes.Ceq)
        | Cgt       -> ilGenerator.Emit(OpCodes.Cgt)
        | Clt       -> ilGenerator.Emit(OpCodes.Clt)
        | Label(l)  -> ilGenerator.MarkLabel(getLabel l)
        | Ldarg(i)  -> ilGenerator.Emit(OpCodes.Ldarg, i)
        | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | Ldloc(i)  -> ilGenerator.Emit(OpCodes.Ldloc, i)
        | Ldsfld(v) -> ilGenerator.Emit(OpCodes.Ldfld, fieldMappings.[v])
        | Mul       -> ilGenerator.Emit(OpCodes.Mul)
        | Ret       -> ilGenerator.Emit(OpCodes.Ret)
        | Starg(i)  -> ilGenerator.Emit(OpCodes.Starg, i)
        | Stsfld(v) -> ilGenerator.Emit(OpCodes.Stfld, fieldMappings.[v])
        | Stloc(i)  -> ilGenerator.Emit(OpCodes.Stloc, i)
        | Sub       -> ilGenerator.Emit(OpCodes.Sub)

    let emitLocal (ilGenerator : ILGenerator) variable =
        ilGenerator.DeclareLocal(variable.Type).SetLocalSymInfo(variable.Name)

    member x.Generate() =
        methodBuilder.SetReturnType ilMethod.ReturnType
        methodBuilder.SetParameters (List.toArray (ilMethod.Parameters |> List.map (fun p -> p.Type)))
        
        let defineParameter index name = 
            methodBuilder.DefineParameter(index, ParameterAttributes.In, name) |> ignore
        ilMethod.Parameters |> List.iteri (fun i p -> defineParameter (i + 1) p.Name)

        ilMethod.Locals |> List.iter (emitLocal ilGenerator)
        ilMethod.Body |> List.iter (emitOpCode ilGenerator)

type CodeGenerator(moduleBuilder : ModuleBuilder, ilClass : ILClass, moduleName : string) =
    let fieldMappings = new FieldMappingDictionary()

    let generateField (typeBuilder : TypeBuilder) (ilField : ILVariable) =
        let fieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
        let fieldBuilder = typeBuilder.DefineField(ilField.Name, ilField.Type, fieldAttributes)
        fieldMappings.Add(ilField, fieldBuilder)

    member x.GenerateType() =
        let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
        let typeBuilder = moduleBuilder.DefineType(moduleName + ".Program", typeAttributes)

        ilClass.Fields  |> List.iter (generateField  typeBuilder)

        let methodMappings = new MethodMappingDictionary()
        let generateMethod ilMethod =
            let methodGenerator = new MethodGenerator(typeBuilder, ilMethod, methodMappings, fieldMappings)
            methodGenerator.Generate()
        ilClass.Methods |> List.iter generateMethod

        (typeBuilder.CreateType(), typeBuilder.GetMethod("main")) // TODO