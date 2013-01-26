module MiniC.Compiler.Tests.SymbolTableTests

open NUnit.Framework
open MiniC.Compiler
open MiniC.Compiler.SemanticAnalysis

[<Test>]
let ``can find declaration in symbol table``() =
    // Arrange.
    let variableDeclarationA = Ast.StaticVariableDeclaration (Ast.ScalarVariableDeclaration(Ast.Float, "a"))
    let localDeclarationA1 = Ast.ScalarVariableDeclaration(Ast.Int, "a")
    let localDeclarationB1 = Ast.ScalarVariableDeclaration(Ast.Bool, "b")
    let localDeclarationA2 = Ast.ScalarVariableDeclaration(Ast.Int, "a")
    let localDeclarationC1 = Ast.ArrayVariableDeclaration(Ast.Int, "c")
    let identifier1 = Ast.IdentifierRef "a"
    let identifier2 = Ast.IdentifierRef "a"
    let identifierExpression1 = Ast.IdentifierExpression identifier1
    let identifierExpression2 = Ast.IdentifierExpression identifier2
    let program =
        [
            variableDeclarationA;
            Ast.FunctionDeclaration(
                Ast.Int, "main",
                [ Ast.ScalarVariableDeclaration(Ast.Int, "a")], // Shadows previous variable
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
    let symbolTable = new SymbolTable(program)

    // Act.
    let result1 = symbolTable.[identifier1]
    let result2 = symbolTable.[identifier2]

    // Assert.
    Assert.That(result1, Is.SameAs(localDeclarationA2))
    Assert.That(result2, Is.SameAs(localDeclarationA1))