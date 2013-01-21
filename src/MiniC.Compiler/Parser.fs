module MiniC.Compiler.Parser

open Piglet.Parser
open Ast
open ParsingUtilities

let configurator = ParserFactory.Configure<obj>()

// Non-terminals

let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())

let program               = nonTerminal<Program>()
let declarationList       = nonTerminal<Declaration list>()
let declaration           = nonTerminal<Declaration>()
let variableDeclaration   = nonTerminal<VariableDeclaration>()
let functionDeclaration   = nonTerminal<FunctionDeclaration>()
let typeSpec              = nonTerminal<TypeSpec>()
let compoundStatement     = nonTerminal<CompoundStatement>()
let parameters            = nonTerminal<Parameters>()
let parameterList         = nonTerminal<Parameters>()
let parameter             = nonTerminal<Parameter>()
let optionalStatementList = nonTerminal<Statement list>()
let statementList         = nonTerminal<Statement list>()
let statement             = nonTerminal<Statement>()
let expressionStatement   = nonTerminal<ExpressionStatement>()
let expression            = nonTerminal<Expression>()
let ifStatement           = nonTerminal<IfStatement>()
let returnStatement       = nonTerminal<Expression option>()

// Terminals

let terminalParse<'T> regex (onParse : (string -> 'T)) =
    new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

let terminal regex =
    new TerminalWrapper<string>(configurator.CreateTerminal(regex))

let ifKeyword     = terminal      "if"
let elseKeyword   = terminal      "else"
let returnKeyword = terminal      "return"
let voidKeyword   = terminal      "void"
let plus          = terminal      @"\+"
let minus         = terminal      "-"
let exclamation   = terminal      @"!"
let asterisk      = terminal      @"\*"
let number        = terminalParse @"\d+"  (fun s -> Ast.IntLiteral(int32 s))
let trueLiteral   = terminalParse "true"  (fun s -> Ast.BoolLiteral(true))
let falseLiteral  = terminalParse "false" (fun s -> Ast.BoolLiteral(false))
let boolKeyword   = terminalParse "bool"  (fun s -> Ast.Bool)
let intKeyword    = terminalParse "int"   (fun s -> Ast.Int)
let identifier    = terminalParse @"\w+"  (fun s -> s)
let openParen     = terminal      @"\("
let closeParen    = terminal      @"\)"
let openCurly     = terminal      @"\{"
let closeCurly    = terminal      @"\}"
let semicolon     = terminal      ";"
let comma         = terminal      ","
let percent       = terminal      "%"
let forwardSlash  = terminal      "/"

// Productions

program.AddProduction(declarationList).SetReduceToFirst()

declarationList.AddProduction(declarationList, declaration).SetReduceFunction (fun x y -> x @ [y])
declarationList.AddProduction(declaration).SetReduceFunction (fun x -> [x])

declaration.AddProduction(variableDeclaration).SetReduceFunction (fun x -> Ast.VariableDeclaration x)
declaration.AddProduction(functionDeclaration).SetReduceFunction (fun x -> Ast.FunctionDeclaration x)

typeSpec.AddProduction(voidKeyword).SetReduceFunction (fun x -> Ast.Void)
typeSpec.AddProduction(boolKeyword).SetReduceToFirst()
typeSpec.AddProduction(intKeyword).SetReduceToFirst()

variableDeclaration.AddProduction(typeSpec, identifier, semicolon)
    .SetReduceFunction (fun x y z -> Ast.ScalarVariableDeclaration(x, y))

functionDeclaration.AddProduction(typeSpec, identifier, openParen, parameters, closeParen, compoundStatement)
    .SetReduceFunction (fun u v w x y z -> (u, v, x, z))

parameters.AddProduction(parameterList).SetReduceToFirst()
parameters.AddProduction(voidKeyword).SetReduceFunction (fun x -> [])

parameterList.AddProduction(parameterList, comma, parameter).SetReduceFunction (fun x y z -> x @ [z])
parameterList.AddProduction(parameter)                      .SetReduceFunction (fun x -> [x])

parameter.AddProduction(typeSpec, identifier).SetReduceFunction (fun x y -> Ast.ScalarParameter(x, y))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction().SetReduceFunction (fun () -> [])

statementList.AddProduction(statementList, statement).SetReduceFunction (fun x y -> x @ [y])
statementList.AddProduction(statement)               .SetReduceFunction (fun x -> [x])

statement.AddProduction(expressionStatement).SetReduceFunction (fun x -> Ast.ExpressionStatement x)
statement.AddProduction(ifStatement)        .SetReduceFunction (fun x -> Ast.IfStatement x)
statement.AddProduction(returnStatement)    .SetReduceFunction (fun x -> Ast.ReturnStatement x)

expressionStatement.AddProduction(expression, semicolon).SetReduceFunction (fun x y -> Ast.Expression x)
expressionStatement.AddProduction(semicolon)            .SetReduceFunction (fun x -> Ast.Nop)

compoundStatement.AddProduction(openCurly, optionalStatementList, closeCurly)
    .SetReduceFunction (fun x y z -> (None, y))

ifStatement.AddProduction(ifKeyword, openParen, expression, closeParen, statement)
    .SetReduceFunction (fun u v w x y -> (w, y, None))

returnStatement.AddProduction(returnKeyword, expression, semicolon).SetReduceFunction (fun x y z -> Some y)
returnStatement.AddProduction(returnKeyword, semicolon)            .SetReduceFunction (fun x y -> None)

// TODO: Extract the binary operators into a binaryOperator non-terminal.
// But can't seem to do it without a shift/reduce conflict.
expression.AddProduction(expression, plus, expression)
    .SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Add, z))
expression.AddProduction(expression, minus, expression)
    .SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Subtract, z))
expression.AddProduction(expression, asterisk, expression)
    .SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Multiply, z))
expression.AddProduction(expression, forwardSlash, expression)
    .SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Divide, z))
expression.AddProduction(expression, percent, expression)
    .SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Modulus, z))

expression.AddProduction(exclamation, expression)
    .SetReduceFunction (fun x y -> Ast.UnaryExpression(Ast.LogicalNegate, y))
expression.AddProduction(minus, expression)
    .SetReduceFunction (fun x y -> Ast.UnaryExpression(Ast.Negate, y))
expression.AddProduction(plus, expression)
    .SetReduceFunction (fun x y -> Ast.UnaryExpression(Ast.Identity, y))

expression.AddProduction(identifier).SetReduceFunction (fun x -> Ast.IdentifierExpression x)
expression.AddProduction(number)    .SetReduceFunction (fun x -> Ast.LiteralExpression x)

configurator.LeftAssociative(downcast exclamation.Symbol, downcast plus.Symbol, downcast minus.Symbol) |> ignore
configurator.LeftAssociative(downcast asterisk.Symbol, downcast forwardSlash.Symbol, downcast percent.Symbol) |> ignore

configurator.LexerSettings.Ignore <- [|@"\s+"|]
let parser = configurator.CreateParser()

let parse (s : string) = parser.Parse(s)