module MiniC.Compiler.Tests.CompilerTests

open System.Reflection
open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can generate and run .NET assembly``() =
    let code = "int main(void) { return 123; }"
    let (compiledType, entryPoint) = Compiler.compileToMemory (new AssemblyName "Foo") code
    
    Assert.That(compiledType, Is.Not.Null)
    Assert.That(entryPoint, Is.Not.Null)

    // Use reflection to execute.
    let result = entryPoint.Invoke(None, Array.empty)
    Assert.That(result, Is.EqualTo(123))