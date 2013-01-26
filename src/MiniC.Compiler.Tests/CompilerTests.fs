module MiniC.Compiler.Tests.CompilerTests

open System.Diagnostics
open System.IO
open System.Reflection
open NUnit.Framework
open MiniC.Compiler

[<SetUpAttribute>]
let cleanup() =
    if Directory.Exists "Binaries" then
        Directory.Delete("Binaries", true)

[<Test>]
let canCompileAndRunAssemblyInMemory() =
    let code = "int main(void) { return 123; }"
    let (compiledType, entryPoint) = Compiler.compileToMemory (new AssemblyName "Foo") code
    
    Assert.That(compiledType, Is.Not.Null)
    Assert.That(entryPoint, Is.Not.Null)

    // Use reflection to execute.
    let result = entryPoint.Invoke(None, Array.empty)
    Assert.That(result, Is.EqualTo(123))

[<TestCase("test1.minic", Result = -5)>]
[<TestCase("test2.minic", Result = 55)>]
[<TestCase("test3.minic", Result = 3)>]
[<TestCase("test4.minic", Result = 9)>]
[<TestCase("test5.minic", Result = 127)>]
[<TestCase("test6.minic", Result = 18)>]
let canCompileSaveAndExecuteConsoleApplicationWithCorrectReturnValue sourceFile =
    let code = File.ReadAllText(Path.Combine("Sources", sourceFile))
    let targetFileName = Path.Combine("Sources", Path.GetFileNameWithoutExtension(sourceFile) + ".exe")
    Compiler.compileToFile targetFileName code

    let testProcess = Process.Start(targetFileName)
    testProcess.WaitForExit()
    
    testProcess.ExitCode