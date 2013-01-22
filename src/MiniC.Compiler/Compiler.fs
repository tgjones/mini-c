module MiniC.Compiler.Compiler

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

let compileToMemory assemblyName code =
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule (assemblyName.Name, assemblyName.Name + ".exe")

    let program = Parser.parse code
    let ilClass = ILBuilder.buildClass program

    let compiledType = CodeGenerator.generateType moduleBuilder ilClass assemblyName.Name
    (assemblyBuilder, compiledType, compiledType.GetMethod("main")) // TODO

let compileToFile fileName code =
    let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension fileName)
    let (a, _, _) = compileToMemory assemblyName code
    a.Save fileName