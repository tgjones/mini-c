module MiniC.Compiler.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Ast

let compile (typeBuilder : TypeBuilder) code =
    let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public)
    let ilGenerator = methodBuilder.GetILGenerator()
    ilGenerator.Emit OpCodes.Ret
    methodBuilder

let compileToMemory assemblyName code =
    let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
    let m = a.DefineDynamicModule (assemblyName.Name, assemblyName.Name + ".exe")
    let typeBuilder = m.DefineType(assemblyName.Name + ".Program", TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public)
    
    let entryPoint = compile typeBuilder code
    a.SetEntryPoint(entryPoint)

    let compiledType = typeBuilder.CreateType()
    (compiledType, compiledType.GetMethod(entryPoint.Name))