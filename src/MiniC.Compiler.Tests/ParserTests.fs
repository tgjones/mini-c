module MiniC.Compiler.Tests.ParserTests

open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can parse empty main method``() =
    let result = Parser.parse "void main(void) { }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], (None, [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse empty main method with random whitespace``() =
    let result = Parser.parse "   void  main (  void ) {   }   "
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], (None, [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse nearly-empty main method``() =
    let result = Parser.parse "
        void main(void) {
            ;
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], (None, [Ast.ExpressionStatement(Ast.Nop)])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse binary expression``() =
    let result = Parser.parse "
        void main(void) {
            123+456;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Void, "main", [],
            (None, [
                Ast.ExpressionStatement(
                    Ast.Expression(
                        Ast.BinaryExpression(
                            Ast.LiteralExpression(Ast.IntLiteral(123)),
                            Ast.Add,
                            Ast.LiteralExpression(Ast.IntLiteral(456))
                       )
                    )
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse return void statement``() =
    let result = Parser.parse "
        void main(void) {
            return;
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], (None, [Ast.ReturnStatement(None)])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse return expression statement``() =
    let result = Parser.parse "
        int foo(void) {
            return 123;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "foo", [],
            (None, [
                Ast.ReturnStatement(
                    Some(Ast.LiteralExpression(Ast.IntLiteral(123)))
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse parameters and expression``() =
    let result = Parser.parse "
        int foo(int a) {
            return a*123 + 456;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "foo",
            [Ast.ScalarParameter (Ast.Int, "a")],
            (None, [
                Ast.ReturnStatement(
                    Some(
                        Ast.BinaryExpression(
                            Ast.BinaryExpression(
                                Ast.IdentifierExpression("a"),
                                Ast.Multiply,
                                Ast.LiteralExpression(Ast.IntLiteral(123))
                            ),
                            Ast.Add,
                            Ast.LiteralExpression(Ast.IntLiteral(456))
                        )
                    )
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse if statement``() =
    let result = Parser.parse "
        int bar(int a, bool b) {
            if (b)
                return a;
            return 1234*a;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int,
            "bar",
            [
                Ast.ScalarParameter (Ast.Int, "a")
                Ast.ScalarParameter (Ast.Bool, "b")
            ],
            (None, [
                Ast.IfStatement(
                    Ast.IdentifierExpression "b",
                    Ast.ReturnStatement(Some(Ast.IdentifierExpression "a")),
                    None
                )
                Ast.ReturnStatement(
                    Some(
                        Ast.BinaryExpression(
                            Ast.LiteralExpression(Ast.IntLiteral(1234)),
                            Ast.Multiply,
                            Ast.IdentifierExpression("a")
                        )
                    )
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))
    
[<Test>]
let ``can parse complex arithmetic expression``() =
    let result = Parser.parse "
        int z;

        int baz(int a, int b) { 
            return 1234*z % 456/b + 789;
        }"
    let expected =
        [
            Ast.VariableDeclaration(Ast.ScalarVariableDeclaration(Ast.Int, "z"))
            Ast.FunctionDeclaration(
                Ast.Int, "baz",
                [
                    Ast.ScalarParameter (Ast.Int, "a")
                    Ast.ScalarParameter (Ast.Int, "b")
                ],
                (None, [
                    Ast.ReturnStatement(
                        Some(
                            Ast.BinaryExpression(
                                Ast.BinaryExpression(
                                    Ast.BinaryExpression(
                                        Ast.BinaryExpression(
                                            Ast.LiteralExpression(Ast.IntLiteral(1234)),
                                            Ast.Multiply,
                                            Ast.IdentifierExpression("z")
                                        ),
                                        Ast.Modulus,
                                        Ast.LiteralExpression(Ast.IntLiteral(456))
                                    ),
                                    Ast.Divide,
                                    Ast.IdentifierExpression("b")
                                ),
                                Ast.Add,
                                Ast.LiteralExpression(Ast.IntLiteral(789))
                            )
                        )
                    )
                ])
            )
        ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse logical negation and unary subtraction expression``() =
    let result = Parser.parse "
        int foo(bool b) {
            if (!b)
                return -1234*a;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int,
            "foo",
            [ Ast.ScalarParameter (Ast.Bool, "b") ],
            (None, [
                Ast.IfStatement(
                    Ast.UnaryExpression (Ast.LogicalNegate, Ast.IdentifierExpression("b")),
                    Ast.ReturnStatement(
                        Some(
                            Ast.UnaryExpression(
                                Ast.Negate,
                                Ast.BinaryExpression(
                                    Ast.LiteralExpression(Ast.IntLiteral(1234)),
                                    Ast.Multiply,
                                    Ast.IdentifierExpression("a")
                                )
                            )
                        )
                    ),
                    None
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse if / else statement``() =
    let result = Parser.parse "
        int main(int a, bool b) {
            if (b)
                return a;
            else
                return 1;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int,
            "main",
            [
                Ast.ScalarParameter (Ast.Int, "a")
                Ast.ScalarParameter (Ast.Bool, "b")
            ],
            (None, [
                Ast.IfStatement(
                    Ast.IdentifierExpression "b",
                    Ast.ReturnStatement(Some(Ast.IdentifierExpression "a")),
                    Some(Ast.ReturnStatement(Some(Ast.LiteralExpression(Ast.IntLiteral(1)))))
                )
            ])
        )]
    Assert.That(result, Is.EqualTo(expected))