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
    let identifierExpression = Ast.IdentifierExpression "a"
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
                            [ Ast.ExpressionStatement (Ast.Expression identifierExpression) ]
                        )
                    ]
                )
            )
        ]
    let symbolTable = SymbolEnvironment.create program

    // Act.
    let result = SymbolEnvironment.findDeclaration identifierExpression symbolTable

    // Assert.
    Assert.That(result, Is.EqualTo(localDeclarationA2))