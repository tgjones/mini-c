module MiniC.Compiler.Tests.ILBuilderTests

open NUnit.Framework
open MiniC.Compiler
open MiniC.Compiler.IL

[<Test>]
let ``can build int return value``() =
    let program =
        [
            Ast.FunctionDeclaration(
                Ast.Void, "main", [],
                ([], [ Ast.ReturnStatement(
                        Some (Ast.LiteralExpression (Ast.IntLiteral 123))
                    )
                ])
            )
        ]
    let semanticAnalysisResult = SemanticAnalysis.analyze program
    let ilBuilder = new ILBuilder(semanticAnalysisResult)
    let result = ilBuilder.BuildClass program
    let expected =
        {
            Name       = "main";
            ReturnType = typeof<System.Void>;
            Parameters = [];
            Locals     = [];
            Body       = [ IL.Ldc_I4(123); IL.Ret ];
        }
    Assert.That(result.Fields, Is.Empty)
    Assert.That(result.Methods, Has.Length.EqualTo 5)
    Assert.That(result.Methods.[4], Is.EqualTo expected)

[<Test>]
let ``can build binary expression``() =
    let program =
        [
            Ast.FunctionDeclaration(
                Ast.Void, "main", [],
                (
                    [],
                    [
                        Ast.ReturnStatement(
                            Some (
                                Ast.BinaryExpression(
                                    Ast.LiteralExpression (Ast.IntLiteral 123),
                                    Ast.Add,
                                    Ast.LiteralExpression (Ast.IntLiteral 456)
                                )
                            )
                        )
                    ]
                )
            )
        ]
    let semanticAnalysisResult = SemanticAnalysis.analyze program
    let ilBuilder = new ILBuilder(semanticAnalysisResult)
    let result = ilBuilder.BuildClass program
    let expected =
        {
            Name       = "main";
            ReturnType = typeof<System.Void>;
            Parameters = [];
            Locals     = [];
            Body       = [ IL.Ldc_I4(123); IL.Ldc_I4(456); IL.Add; IL.Ret ];
        }
    Assert.That(result.Fields, Is.Empty)
    Assert.That(result.Methods, Has.Length.EqualTo 5)
    Assert.That(result.Methods.[4], Is.EqualTo expected)