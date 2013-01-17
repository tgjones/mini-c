module MiniC.Compiler.Tests.ParserTests

open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can parse empty main method``() =
    let result = Parser.parse "void main(void) { }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse empty main method with random whitespace``() =
    let result = Parser.parse "   void  main (  void ) {   }   "
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse nearly-empty main method``() =
    let result = Parser.parse "
        void main(void) {
            ;
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [Ast.ExpressionStatement(Ast.Nop)])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse binary expression``() =
    let result = Parser.parse "
        void main(void) {
            123+456
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", None, (None, [Ast.ExpressionStatement(Ast.Nop)])) ]
    Assert.That(result, Is.EqualTo(expected))