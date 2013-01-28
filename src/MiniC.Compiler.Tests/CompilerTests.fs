module MiniC.Compiler.Tests.CompilerTests

open System
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
let ``can compile and run assembly in memory``() =
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
[<TestCase("test6.minic", Result = 15)>]
let ``can compile, save and execute console application with correct return value`` sourceFile =
    let code = File.ReadAllText(Path.Combine("Sources", sourceFile))
    let targetFileName = Path.Combine("Sources", Path.GetFileNameWithoutExtension(sourceFile) + ".exe")
    Compiler.compileToFile targetFileName code

    let testProcess = Process.Start(targetFileName)
    testProcess.WaitForExit()
    
    testProcess.ExitCode

[<Test>]
let ``can compile, save and execute application with I/O``() =
    let sourceFile = "test7.minic"
    let code = File.ReadAllText(Path.Combine("Sources", sourceFile))
    let targetFileName = Path.Combine("Sources", Path.GetFileNameWithoutExtension(sourceFile) + ".exe")
    Compiler.compileToFile targetFileName code

    let processStartInfo = ProcessStartInfo(targetFileName)
    processStartInfo.RedirectStandardInput <- true
    processStartInfo.RedirectStandardOutput <- true
    processStartInfo.UseShellExecute <- false
    let testProcess = Process.Start(processStartInfo)

    let output = ref ""
    testProcess.OutputDataReceived.Add (fun args -> output := !output + args.Data + "\n")

    testProcess.BeginOutputReadLine()

    let inputStream = testProcess.StandardInput
    inputStream.WriteLine "9"
    inputStream.WriteLine "200"

    inputStream.Close()

    testProcess.WaitForExit()
    
    Assert.That(testProcess.ExitCode, Is.EqualTo 0)
    Assert.That(!output, Is.EqualTo ("3\n400\n\n"))

[<TestCase("error1.minic", "CS003 A variable named 'x' is already defined in this scope")>]
[<TestCase("error2.minic", "CS003 A variable named 'y' is already defined in this scope")>]
[<TestCase("error3.minic", "CS001 Lexer error: Invalid character '^'")>]
[<TestCase("error4.minic", "CS004 Cannot convert type 'bool' to 'int'")>]
[<TestCase("error5.minic", "CS002 Parser error: Illegal token [a-zA-Z_][a-zA-Z_0-9]*. Expected \+,-,\*,\(,\),\[,\],;,,,%,/,=,\|\|,==,!=,<=,<,>=,>,&&,\.")>]
[<TestCase("error6.minic", "CS005 Operator '<' cannot be applied to operands of type 'bool' and 'int'")>]
[<TestCase("error7.minic", "CS005 Operator '!=' cannot be applied to operands of type 'bool' and 'int'")>]
[<TestCase("error8.minic", "CS005 Operator '||' cannot be applied to operands of type 'bool' and 'int'")>]
[<TestCase("error9.minic", "CS006 The name 'foo' does not exist in the current context")>]
[<TestCase("error10.minic", "CS004 Cannot convert type 'bool' to 'void'")>]
[<TestCase("error11.minic", "CS007 Call to function 'test' has some invalid arguments. Argument 1: Cannot convert from 'int' to 'bool'")>]
[<TestCase("error12.minic", "CS008 Function 'test' takes 1 arguments, but here was given 2")>]
[<TestCase("error13.minic", "CS006 The name 'foo' does not exist in the current context")>]
[<TestCase("error14.minic", "CS009 No enclosing loop out of which to break")>]
[<TestCase("error15.minic", "CS004 Cannot convert type 'void' to 'int'")>]
[<TestCase("error16.minic", "CS004 Cannot convert type 'bool' to 'int'")>]
[<TestCase("error17.minic", "CS004 Cannot convert type 'float' to 'int'")>]
[<TestCase("error18.minic", "CS004 Cannot convert type 'bool' to 'int'")>]
[<TestCase("error19.minic", "CS004 Cannot convert type 'bool' to 'int[]'")>]
[<TestCase("error20.minic", "CS004 Cannot convert type 'bool' to 'int'")>]
[<TestCase("error21.minic", "CS004 Cannot convert type 'int' to 'int[]'")>]
let ``can detect semantic errors`` sourceFile (compilerError : string) =
    let code = File.ReadAllText(Path.Combine("Sources", sourceFile))
    Assert.That(
        (fun () -> Compiler.compileToMemory (AssemblyName(Path.GetFileNameWithoutExtension sourceFile)) code |> ignore),
        Throws.TypeOf<CompilerException>().With.Message.EqualTo compilerError)