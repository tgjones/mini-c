module MiniC.Compiler.Tests.ParserTests

open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can parse empty main method``() =
    let result = Parser.parse "void main(void) { ; }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [])) ]
    Assert.That(result, Is.EqualTo(expected))