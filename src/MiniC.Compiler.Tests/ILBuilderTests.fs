module MiniC.Compiler.Tests.ILBuilderTests

open NUnit.Framework
open MiniC.Compiler
open MiniC.Compiler.IL

//[<Test>]
//let ``can build intermediate representation of empty main method``() =
//    let program  = [ Ast.FunctionDeclaration(Ast.Void, "main", [], ([], [])) ]
//    let result = ILBuilder.buildClass program
//    let expected =
//        {
//            Fields = [];
//            Methods =
//                [
//                    {
//                        Name       = "main";
//                        ReturnType = typeof<unit>;
//                        Parameters = [];
//                        Body       = [];
//                    }
//                ];
//        }
//    Assert.That(result, Is.EqualTo(expected))