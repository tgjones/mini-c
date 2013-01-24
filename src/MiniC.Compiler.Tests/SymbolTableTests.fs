module MiniC.Compiler.Tests.SymbolTableTests

open NUnit.Framework
open MiniC.Compiler

[<Test>]
let ``can find declaration in symbol table``() =
    // Arrange.
    let variableDeclarationA = Ast.VariableDeclaration (Ast.ScalarVariableDeclaration(Ast.Float, "a"))
    let localDeclarationA1 = Ast.ScalarLocalDeclaration(Ast.Int, "a")
    let localDeclarationB1 = Ast.ScalarLocalDeclaration(Ast.Bool, "b")
    let localDeclarationA2 = Ast.ScalarLocalDeclaration(Ast.Int, "a")
    let localDeclarationC1 = Ast.ArrayLocalDeclaration(Ast.Int, "c")
    let identifierExpression1 = Ast.IdentifierExpression "a"
    let identifierExpression2 = Ast.IdentifierExpression "a"
    let program =
        [
            variableDeclarationA;
            Ast.FunctionDeclaration(
                Ast.Int, "main",
                [ Ast.ScalarParameter(Ast.Int, "a")], // Shadows previous variable
                (
                    [
                        localDeclarationA1; // Shadows previous parameter
                        localDeclarationB1
                    ],
                    [
                        Ast.CompoundStatement(
                            [
                                localDeclarationA2; // Shadows previous local
                                localDeclarationC1
                            ],
                            [ Ast.ExpressionStatement (Ast.Expression identifierExpression1) ]
                        );
                        Ast.ExpressionStatement (Ast.Expression identifierExpression2)
                    ]
                )
            )
        ]
    let symbolEnvironment = SymbolEnvironment.create program

    // Act.
    let result1 = SymbolEnvironment.findDeclaration identifierExpression1 symbolEnvironment
    let result2 = SymbolEnvironment.findDeclaration identifierExpression2 symbolEnvironment

    // Assert.
    Assert.That(result1, Is.SameAs(localDeclarationA2))
    Assert.That(result2, Is.SameAs(localDeclarationA1))