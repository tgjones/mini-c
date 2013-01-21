module MiniC.Compiler.Tests.ParserTests

open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can parse empty main method``() =
    let result = Parser.parse "void main(void) { }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], ([], [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse empty main method with random whitespace``() =
    let result = Parser.parse "   void  main (  void ) {   }   "
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], ([], [])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse nearly-empty main method``() =
    let result = Parser.parse "
        void main(void) {
            ;
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], ([], [Ast.ExpressionStatement(Ast.Nop)])) ]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse binary arithmetic expressions``() =
    let doTest operator binaryOperator =
        let code = (sprintf "
            void main(void) {
                123%s456;
            }" operator)
        let result = Parser.parse code
        let expected =
            [Ast.FunctionDeclaration(
                Ast.Void, "main", [],
                (
                    [],
                    [
                        Ast.ExpressionStatement(
                            Ast.Expression(
                                Ast.BinaryExpression(
                                    Ast.LiteralExpression(Ast.IntLiteral(123)),
                                    binaryOperator,
                                    Ast.LiteralExpression(Ast.IntLiteral(456))
                               )
                            )
                        )
                    ]
                )
            )]
        Assert.That(result, Is.EqualTo(expected))

    doTest "+" Ast.Add
    doTest "-" Ast.Subtract
    doTest "*" Ast.Multiply
    doTest "/" Ast.Divide
    doTest "%" Ast.Modulus

[<Test>]
let ``can parse unary arithmetic expressions``() =
    let doTest operator unaryOperator =
        let code = (sprintf "
            void main(void) {
                %s123;
            }" operator)
        let result = Parser.parse code
        let expected =
            [Ast.FunctionDeclaration(
                Ast.Void, "main", [],
                (
                    [],
                    [
                        Ast.ExpressionStatement(
                            Ast.Expression(
                                Ast.UnaryExpression(
                                    unaryOperator,
                                    Ast.LiteralExpression(Ast.IntLiteral(123))
                               )
                            )
                        )
                    ]
                )
            )]
        Assert.That(result, Is.EqualTo(expected))

    doTest "!" Ast.LogicalNegate
    doTest "-" Ast.Negate
    doTest "+" Ast.Identity

[<Test>]
let ``can parse return void statement``() =
    let result = Parser.parse "
        void main(void) {
            return;
        }"
    let expected =
        [Ast.FunctionDeclaration(Ast.Void, "main", [], ([], [Ast.ReturnStatement(None)])) ]
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
            (
                [],
                [
                    Ast.ReturnStatement(
                        Some(Ast.LiteralExpression(Ast.IntLiteral(123)))
                    )
                ]
            )
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
            (
                [],
                [
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
                ]
            )
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
            (
                [],
                [
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
                ]
            )
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
                (
                    [],
                    [
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
                    ]
                )
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
            (
                [],
                [
                    Ast.IfStatement(
                        Ast.UnaryExpression (Ast.LogicalNegate, Ast.IdentifierExpression("b")),
                        Ast.ReturnStatement(
                            Some(
                                Ast.BinaryExpression(
                                    Ast.UnaryExpression(
                                        Ast.Negate,
                                        Ast.LiteralExpression(Ast.IntLiteral(1234))
                                    ),
                                    Ast.Multiply,
                                    Ast.IdentifierExpression("a")
                                )
                            )
                        ),
                        None
                    )
                ]
            )
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
            (
                [],
                [
                    Ast.IfStatement(
                        Ast.IdentifierExpression "b",
                        Ast.ReturnStatement(Some(Ast.IdentifierExpression "a")),
                        Some(Ast.ReturnStatement(Some(Ast.LiteralExpression (Ast.IntLiteral 1))))
                    )
                ]
            )
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse while statement``() =
    let result = Parser.parse "
        int main(bool b) {
            while (b)
                return 1;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "main",
            [ Ast.ScalarParameter (Ast.Bool, "b") ],
            (
                [],
                [
                    Ast.WhileStatement(
                        Ast.IdentifierExpression "b",
                        Ast.ReturnStatement(Some(Ast.LiteralExpression (Ast.IntLiteral 1)))
                    )
                ]
            )
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse local declarations in compound statements``() =
    let result = Parser.parse "
        int main(void) {
            int a;
            bool b;
            float c;
            {
                int d;
            }
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "main", [],
            (
                [
                    Ast.ScalarLocalDeclaration(Ast.Int, "a")
                    Ast.ScalarLocalDeclaration(Ast.Bool, "b")
                    Ast.ScalarLocalDeclaration(Ast.Float, "c")
                ],
                [
                    Ast.CompoundStatement(
                        [ Ast.ScalarLocalDeclaration(Ast.Int, "d") ],
                        []
                    )
                ]
            )
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse function call``() =
    let result = Parser.parse "
        int func(int i, int j) {
            return i + j;
        }

        int main(int i) {
            return func(i + 1, i - 1);
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "main",
            [ Ast.ScalarParameter (Ast.Bool, "b") ],
            (
                [],
                [
                    Ast.WhileStatement(
                        Ast.IdentifierExpression "b",
                        Ast.ReturnStatement(Some(Ast.LiteralExpression (Ast.IntLiteral 1)))
                    )
                ]
            )
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse assignment expression``() =
    let result = Parser.parse "
        int main(int i) {
            i = i + 1;
            return i;
        }"
    let expected =
        [Ast.FunctionDeclaration(
            Ast.Int, "main",
            [ Ast.ScalarParameter (Ast.Int, "i") ],
            (
                [],
                [
                    Ast.ExpressionStatement(
                        Ast.Expression(
                            Ast.AssignmentExpression(
                                "i",
                                Ast.BinaryExpression(
                                    Ast.IdentifierExpression("i"),
                                    Ast.Add,
                                    Ast.LiteralExpression(Ast.IntLiteral(1))
                                )
                            )
                        )
                    )
                    Ast.ReturnStatement(Some(Ast.IdentifierExpression "i"))
                ]
            )
        )]
    Assert.That(result, Is.EqualTo(expected))

[<Test>]
let ``can parse logical comparison expressions``() =
    let doTest operator binaryOperator =
        let code = (sprintf "
            bool main(void) {
                return true %s false;
            }" operator)
        let result = Parser.parse code
        let expected =
            [Ast.FunctionDeclaration(
                Ast.Bool, "main", [],
                (
                    [],
                    [
                        Ast.ReturnStatement(
                            Some(
                                Ast.BinaryExpression(
                                    Ast.LiteralExpression(Ast.BoolLiteral(true)),
                                    binaryOperator,
                                    Ast.LiteralExpression(Ast.BoolLiteral(false))
                                )
                            )
                        )
                    ]
                )
            )]
        Assert.That(result, Is.EqualTo(expected))
    
    doTest "||" Ast.ConditionalOr
    doTest "==" Ast.Equal
    doTest "!=" Ast.NotEqual
    doTest "<=" Ast.LessEqual
    doTest "<"  Ast.Less
    doTest ">=" Ast.GreaterEqual
    doTest ">"  Ast.Greater
    doTest "&&" Ast.ConditionalAnd