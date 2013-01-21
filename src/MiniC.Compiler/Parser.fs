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
let whileStatement        = nonTerminal<WhileStatement>()
let ifStatement           = nonTerminal<IfStatement>()
let optionalElseStatement = nonTerminal<Statement option>()
let returnStatement       = nonTerminal<Expression option>()
let expression            = nonTerminal<Expression>()
let binaryOperator        = nonTerminal<BinaryOperator>()
let unaryOperator         = nonTerminal<UnaryOperator>()

// Terminals

let terminalParse<'T> regex (onParse : (string -> 'T)) =
    new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

let terminal regex =
    new TerminalWrapper<string>(configurator.CreateTerminal(regex))

let ifKeyword     = terminal      "if"
let elseKeyword   = terminal      "else"
let whileKeyword  = terminal      "while"
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

// Precedence

let optionalElsePrecedenceGroup = configurator.LeftAssociative()

configurator.LeftAssociative(downcast elseKeyword.Symbol) |> ignore

configurator.LeftAssociative(downcast exclamation.Symbol, downcast plus.Symbol, downcast minus.Symbol) |> ignore
configurator.LeftAssociative(downcast asterisk.Symbol, downcast forwardSlash.Symbol, downcast percent.Symbol) |> ignore

let binaryExpressionPrecedenceGroup = configurator.LeftAssociative()
let unaryExpressionPrecedenceGroup  = configurator.RightAssociative()

// Productions

program.AddProduction(declarationList).SetReduceToFirst()

declarationList.AddProduction(declarationList, declaration).SetReduceFunction (fun a b -> a @ [b])
declarationList.AddProduction(declaration).SetReduceFunction (fun a -> [a])

declaration.AddProduction(variableDeclaration).SetReduceFunction (fun a -> Ast.VariableDeclaration a)
declaration.AddProduction(functionDeclaration).SetReduceFunction (fun a -> Ast.FunctionDeclaration a)

typeSpec.AddProduction(voidKeyword).SetReduceFunction (fun _ -> Ast.Void)
typeSpec.AddProduction(boolKeyword).SetReduceToFirst()
typeSpec.AddProduction(intKeyword).SetReduceToFirst()

variableDeclaration.AddProduction(typeSpec, identifier, semicolon)
    .SetReduceFunction (fun a b _ -> Ast.ScalarVariableDeclaration(a, b))

functionDeclaration.AddProduction(typeSpec, identifier, openParen, parameters, closeParen, compoundStatement)
    .SetReduceFunction (fun a b _ d _ f -> (a, b, d, f))

parameters.AddProduction(parameterList).SetReduceToFirst()
parameters.AddProduction(voidKeyword).SetReduceFunction (fun _ -> [])

parameterList.AddProduction(parameterList, comma, parameter).SetReduceFunction (fun a _ c -> a @ [c])
parameterList.AddProduction(parameter)                      .SetReduceFunction (fun a -> [a])

parameter.AddProduction(typeSpec, identifier).SetReduceFunction (fun a b -> Ast.ScalarParameter(a, b))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction()             .SetReduceFunction (fun () -> [])

statementList.AddProduction(statementList, statement).SetReduceFunction (fun a b -> a @ [b])
statementList.AddProduction(statement)               .SetReduceFunction (fun a -> [a])

statement.AddProduction(expressionStatement).SetReduceFunction (fun a -> Ast.ExpressionStatement a)
statement.AddProduction(ifStatement)        .SetReduceFunction (fun a -> Ast.IfStatement a)
statement.AddProduction(whileStatement)     .SetReduceFunction (fun a -> Ast.WhileStatement a)
statement.AddProduction(returnStatement)    .SetReduceFunction (fun a -> Ast.ReturnStatement a)

expressionStatement.AddProduction(expression, semicolon).SetReduceFunction (fun a _ -> Ast.Expression a)
expressionStatement.AddProduction(semicolon)            .SetReduceFunction (fun _ -> Ast.Nop)

compoundStatement.AddProduction(openCurly, optionalStatementList, closeCurly)
    .SetReduceFunction (fun _ b _ -> (None, b))

whileStatement.AddProduction(whileKeyword, openParen, expression, closeParen, statement)
    .SetReduceFunction (fun a b c d e -> (c, e))

ifStatement.AddProduction(ifKeyword, openParen, expression, closeParen, statement, optionalElseStatement)
    .SetReduceFunction (fun _ _ c _ e f -> (c, e, f))

let elseStatementProduction = optionalElseStatement.AddProduction(elseKeyword, statement)
elseStatementProduction.SetReduceFunction (fun _ b -> Some b)
elseStatementProduction.SetPrecedence optionalElsePrecedenceGroup

let elseEpsilonProduction = optionalElseStatement.AddProduction()
elseEpsilonProduction.SetReduceFunction (fun () -> None)
elseEpsilonProduction.SetPrecedence optionalElsePrecedenceGroup

returnStatement.AddProduction(returnKeyword, expression, semicolon).SetReduceFunction (fun _ b _ -> Some b)
returnStatement.AddProduction(returnKeyword, semicolon)            .SetReduceFunction (fun _ _ -> None)

let binaryExpressionProduction = expression.AddProduction(expression, binaryOperator, expression)
binaryExpressionProduction.SetReduceFunction (fun a b c -> Ast.BinaryExpression(a, b, c))
binaryExpressionProduction.SetPrecedence binaryExpressionPrecedenceGroup

let unaryExpressionProduction = expression.AddProduction(unaryOperator, expression)
unaryExpressionProduction.SetReduceFunction (fun a b -> Ast.UnaryExpression(a, b))
unaryExpressionProduction.SetPrecedence unaryExpressionPrecedenceGroup

expression.AddProduction(identifier).SetReduceFunction (fun a -> Ast.IdentifierExpression a)
expression.AddProduction(number)    .SetReduceFunction (fun a -> Ast.LiteralExpression a)

binaryOperator.AddProduction(plus)        .SetReduceFunction (fun a -> Ast.Add)
binaryOperator.AddProduction(minus)       .SetReduceFunction (fun a -> Ast.Subtract)
binaryOperator.AddProduction(asterisk)    .SetReduceFunction (fun a -> Ast.Multiply)
binaryOperator.AddProduction(forwardSlash).SetReduceFunction (fun a -> Ast.Divide)
binaryOperator.AddProduction(percent)     .SetReduceFunction (fun a -> Ast.Modulus)

unaryOperator.AddProduction(exclamation).SetReduceFunction (fun a -> Ast.LogicalNegate)
unaryOperator.AddProduction(minus)      .SetReduceFunction (fun a -> Ast.Negate)
unaryOperator.AddProduction(plus)       .SetReduceFunction (fun a -> Ast.Identity)

configurator.LexerSettings.Ignore <- [|@"\s+"|]
let parser = configurator.CreateParser()

let parse (s : string) = parser.Parse(s)