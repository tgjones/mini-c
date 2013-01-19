module MiniC.Compiler.Tests.CompilerTests

open System.Reflection
open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can generate .NET assembly``() =
    let parseTree = [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [])) ]
    let (compiledType, entryPoint) = Compiler.compileToMemory (new AssemblyName "Foo") parseTree
    
    Assert.That(compiledType, Is.Not.Null)
    Assert.That(entryPoint, Is.Not.Null)

    // Use reflection to execute.
    entryPoint.Invoke(None, Array.empty) |> ignore