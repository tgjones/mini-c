module MiniC.Compiler.Parser

open Piglet.Parser
open Ast

let configurator = ParserFactory.Configure<System.Object>()

let program = configurator.CreateNonTerminal()
let declarationList = configurator.CreateNonTerminal()
let declaration = configurator.CreateNonTerminal()
let functionDeclaration = configurator.CreateNonTerminal()
let compoundStatement = configurator.CreateNonTerminal()
let typeSpec = configurator.CreateNonTerminal()
let parameters = configurator.CreateNonTerminal()
let optionalStatementList = configurator.CreateNonTerminal()
let statementList = configurator.CreateNonTerminal()
let statement = configurator.CreateNonTerminal()
let expressionStatement = configurator.CreateNonTerminal()
let expression = configurator.CreateNonTerminal()
let returnStatement = configurator.CreateNonTerminal()

let plus = configurator.CreateTerminal(@"\+")

let number = configurator.CreateTerminal(@"\d+", (fun s -> Ast.IntLiteral(System.Int32.Parse(s)) :> obj))
let trueLiteral = configurator.CreateTerminal("true", (fun s -> box (Ast.BoolLiteral(true))))
let falseLiteral = configurator.CreateTerminal("false", (fun s -> box (Ast.BoolLiteral(false))))
let voidKeyword = configurator.CreateTerminal("void")
let intKeyword = configurator.CreateTerminal("int", (fun s -> box Ast.Int))
let returnKeyword = configurator.CreateTerminal("return")
let identifier2 = configurator.CreateTerminal(@"\w+", (fun s -> s :> obj))

program.AddProduction(declarationList).SetReduceToFirst()
declarationList.AddProduction(declarationList, declaration).SetReduceFunction((fun o ->
    let list = downcast o.[0]
    (list :: downcast o.[1]) :> obj))
declarationList.AddProduction(declaration).SetReduceFunction((fun o ->
    box [unbox<Ast.Declaration> o.[0]]))
declaration.AddProduction(functionDeclaration).SetReduceToFirst()

expression.AddProduction(expression, plus, expression)
    .SetReduceFunction((fun o ->
        let o1 = downcast o.[0]
        let o3 = downcast o.[2]
        (Ast.BinaryExpression(o1, Ast.Add, o3)) :> obj))
expression.AddProduction(number)
    .SetReduceFunction((fun o -> upcast (Ast.LiteralExpression(downcast o.[0]))))

functionDeclaration.AddProduction(typeSpec, identifier2, "(", parameters, ")", compoundStatement).SetReduceFunction(
    (fun o -> Ast.FunctionDeclaration(unbox o.[0],
                                      unbox o.[1],
                                      None,
                                      unbox o.[5]) :> obj))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction().SetReduceFunction((fun o -> box<Ast.Statement list> []))

statementList.AddProduction(statementList, statement).SetReduceFunction((fun o ->
    let list = downcast o.[0]
    (list :: downcast o.[1]) :> obj))
statementList.AddProduction(statement).SetReduceFunction((fun o ->
    box [unbox<Ast.Statement> o.[0]]))

statement.AddProduction(expressionStatement)
    .SetReduceFunction((fun o -> upcast Ast.ExpressionStatement(downcast o.[0])))
statement.AddProduction(returnStatement)
    .SetReduceFunction((fun o -> upcast Ast.ReturnStatement(downcast o.[0])))

expressionStatement.AddProduction(expression, ";")
    .SetReduceFunction((fun o -> upcast Ast.Expression(downcast o.[0])))
expressionStatement.AddProduction(";")
    .SetReduceFunction((fun o -> upcast Ast.Nop))

compoundStatement.AddProduction("{", optionalStatementList, "}")
    .SetReduceFunction((fun o -> box (Option<Ast.LocalDeclarations>.None, unbox<Ast.Statement list> o.[1])))

returnStatement.AddProduction("return", expression, ";")
    .SetReduceFunction((fun o -> upcast Some(unbox<Ast.Expression> o.[1])))
returnStatement.AddProduction("return", ";")
    .SetReduceFunction((fun o -> upcast None))

typeSpec.AddProduction(voidKeyword).SetReduceFunction((fun o -> box Ast.Void))
typeSpec.AddProduction(intKeyword).SetReduceToFirst()

parameters.AddProduction(voidKeyword).SetReduceFunction((fun o -> [] :> obj))

configurator.LeftAssociative(plus) |> ignore

configurator.LexerSettings.Ignore <- [|@"\s+"|]
let parser = configurator.CreateParser()

let parse (s : string) = parser.Parse(s)