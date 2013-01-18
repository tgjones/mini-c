module MiniC.Compiler.Parser

open Piglet.Parser
open Ast

let configurator = ParserFactory.Configure<System.Object>()

// Non-terminals

let nonTerminal = configurator.CreateNonTerminal

let program               = nonTerminal()
let declarationList       = nonTerminal()
let declaration           = nonTerminal()
let functionDeclaration   = nonTerminal()
let compoundStatement     = nonTerminal()
let typeSpec              = nonTerminal()
let parameters            = nonTerminal()
let optionalStatementList = nonTerminal()
let statementList         = nonTerminal()
let statement             = nonTerminal()
let expressionStatement   = nonTerminal()
let expression            = nonTerminal()
let returnStatement       = nonTerminal()

// Terminals

let terminalParse regex onParse =
    configurator.CreateTerminal(regex, (fun s -> box (onParse s)))

let terminal regex =
    configurator.CreateTerminal(regex)

let returnKeyword = terminal      "return"
let voidKeyword   = terminal      "void"
let plus          = terminalParse @"\+"    (fun s -> s)
let number        = terminalParse @"\d+"   (fun s -> Ast.IntLiteral(int32 s))
let trueLiteral   = terminalParse "true"   (fun s -> Ast.BoolLiteral(true))
let falseLiteral  = terminalParse "false"  (fun s -> Ast.BoolLiteral(false))
let intKeyword    = terminalParse "int"    (fun s -> Ast.Int)
let identifier    = terminalParse @"\w+"   (fun s -> s)

// Productions

let addProduction (part : Configuration.ISymbol<'b>)
                  (nonTerminal : Configuration.INonTerminal<'a>) =
    nonTerminal.AddProduction(part)

let addProduction2 (part1 : Configuration.ISymbol<_>)
                   (part2 : Configuration.ISymbol<_>)
                   (nonTerminal : Configuration.INonTerminal<_>) =
    nonTerminal.AddProduction(part1, part2)

let reduceFunction<'T,'U> (f : ('T -> 'V))
                          (production : Configuration.IProduction<_>) =
    production.SetReduceFunction (fun o -> box (f (unbox o.[0])))

let reduceFunction2<'T,'U,'V> (f : ('T -> 'U -> 'V))
                              (production : Configuration.IProduction<_>) =
    production.SetReduceFunction (fun o -> box (f (unbox o.[0]) (unbox o.[1])))

let reduceToFirst (production : Configuration.IProduction<_>) =
    production.SetReduceToFirst()

program
    |> addProduction declarationList
    |> reduceToFirst

declarationList
    |> addProduction2 declarationList declaration
    |> reduceFunction2<Declaration list, Declaration, Declaration list> (fun x y -> x @ [y])
declarationList
    |> addProduction declaration
    |> reduceFunction<Declaration, Declaration list> (fun x -> [x])

declaration.AddProduction(functionDeclaration).SetReduceToFirst()

expression.AddProduction(expression, plus, expression)
    .SetReduceFunction((fun o ->
        let o1 = downcast o.[0]
        let o3 = downcast o.[2]
        (Ast.BinaryExpression(o1, Ast.Add, o3)) :> obj))
expression.AddProduction(number)
    .SetReduceFunction((fun o -> upcast (Ast.LiteralExpression(downcast o.[0]))))

functionDeclaration.AddProduction(typeSpec, identifier, "(", parameters, ")", compoundStatement).SetReduceFunction(
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

returnStatement.AddProduction(returnKeyword, expression, ";")
    .SetReduceFunction((fun o -> upcast Some(unbox<Ast.Expression> o.[1])))
returnStatement.AddProduction(returnKeyword, ";")
    .SetReduceFunction((fun o -> upcast None))

typeSpec.AddProduction(voidKeyword).SetReduceFunction((fun o -> box Ast.Void))
typeSpec.AddProduction(intKeyword).SetReduceToFirst()

parameters.AddProduction(voidKeyword).SetReduceFunction((fun o -> [] :> obj))

configurator.LeftAssociative(plus) |> ignore

configurator.LexerSettings.Ignore <- [|@"\s+"|]
let parser = configurator.CreateParser()

let parse (s : string) = parser.Parse(s)