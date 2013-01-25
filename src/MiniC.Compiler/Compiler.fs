module MiniC.Compiler.Compiler

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

let compile (assemblyBuilder : AssemblyBuilder) code =
    let assemblyName = assemblyBuilder.GetName()
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, assemblyName.Name + ".exe", true)

    let program = Parser.parse code
    let symbolEnvironment = SymbolEnvironment.create program

    let ilBuilder = new ILBuilder(symbolEnvironment)
    let ilClass = ilBuilder.BuildClass program

    let codeGenerator = new CodeGenerator(moduleBuilder, ilClass, assemblyName.Name)
    let (compiledType, entryPoint) = codeGenerator.GenerateType()
    assemblyBuilder.SetEntryPoint entryPoint
    (compiledType, entryPoint)

let compileToMemory assemblyName code =
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            assemblyName, AssemblyBuilderAccess.RunAndSave)
    compile assemblyBuilder code

let compileToFile fileName code =
    let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension fileName)
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            assemblyName, AssemblyBuilderAccess.RunAndSave,
            Path.GetDirectoryName(fileName))
    let (_, _) = compile assemblyBuilder code
    assemblyBuilder.Save (Path.GetFileName fileName)