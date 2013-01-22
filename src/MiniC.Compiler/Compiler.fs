module MiniC.Compiler.Compiler

open System
open System.Reflection
open System.Reflection.Emit

let compileToMemory assemblyName code =
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule (assemblyName.Name, assemblyName.Name + ".exe")

    let program = Parser.parse code
    let ilClass = ILBuilder.buildClass program

    let compiledType = CodeGenerator.generateType moduleBuilder ilClass assemblyName.Name
    (compiledType, compiledType.GetMethod("main")) // TODO