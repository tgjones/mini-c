module MiniC.Compiler.Parser

open Piglet.Parser
open CompilerErrors
open Ast
open ParsingUtilities

let configurator = ParserFactory.Configure<obj>()

// Non-terminals

let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())

let program                   = nonTerminal<Program>()
let declarationList           = nonTerminal<Declaration list>()
let declaration               = nonTerminal<Declaration>()
let staticVariableDeclaration = nonTerminal<VariableDeclaration>()
let functionDeclaration       = nonTerminal<FunctionDeclaration>()
let typeSpec                  = nonTerminal<TypeSpec>()
let parameters                = nonTerminal<Parameters>()
let parameterList             = nonTerminal<Parameters>()
let parameter                 = nonTerminal<VariableDeclaration>()
let optionalStatementList     = nonTerminal<Statement list>()
let statementList             = nonTerminal<Statement list>()
let statement                 = nonTerminal<Statement>()
let expressionStatement       = nonTerminal<ExpressionStatement>()
let whileStatement            = nonTerminal<WhileStatement>()
let compoundStatement         = nonTerminal<CompoundStatement>()
let optionalLocalDeclarations = nonTerminal<VariableDeclaration list>()
let localDeclarations         = nonTerminal<VariableDeclaration list>()
let localDeclaration          = nonTerminal<VariableDeclaration>()
let ifStatement               = nonTerminal<IfStatement>()
let optionalElseStatement     = nonTerminal<Statement option>()
let returnStatement           = nonTerminal<Expression option>()
let breakStatement            = nonTerminal<unit>()
let expression                = nonTerminal<Expression>()
let unaryOperator             = nonTerminal<UnaryOperator>()
let optionalArguments         = nonTerminal<Arguments>()
let arguments                 = nonTerminal<Arguments>()

// Terminals

let terminalParse<'T> regex (onParse : (string -> 'T)) =
    new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

let terminal regex =
    new TerminalWrapper<string>(configurator.CreateTerminal(regex))

let ifKeyword        = terminal      "if"
let elseKeyword      = terminal      "else"
let whileKeyword     = terminal      "while"
let returnKeyword    = terminal      "return"
let breakKeyword     = terminal      "break"
let newKeyword       = terminal      "new"
let sizeKeyword      = terminal      "size"
let voidKeyword      = terminal      "void"
let plus             = terminal      @"\+"
let minus            = terminal      "-"
let exclamation      = terminal      "!"
let asterisk         = terminal      @"\*"
let intLiteral       = terminalParse @"\d+"      (fun s -> Ast.IntLiteral(int32 s))
let floatLiteral     = terminalParse @"\d+\.\d+" (fun s -> Ast.FloatLiteral(float s))
let trueLiteral      = terminalParse "true"      (fun s -> Ast.BoolLiteral(true))
let falseLiteral     = terminalParse "false"     (fun s -> Ast.BoolLiteral(false))
let boolKeyword      = terminalParse "bool"      (fun s -> Ast.Bool)
let intKeyword       = terminalParse "int"       (fun s -> Ast.Int)
let floatKeyword     = terminalParse "float"     (fun s -> Ast.Float)
let identifier       = terminalParse "[a-zA-Z_][a-zA-Z_0-9]*"  (fun s -> s)
let openParen        = terminal      @"\("
let closeParen       = terminal      @"\)"
let openCurly        = terminal      @"\{"
let closeCurly       = terminal      @"\}"
let openSquare       = terminal      @"\["
let closeSquare      = terminal      @"\]"
let semicolon        = terminal      ";"
let comma            = terminal      ","
let percent          = terminal      "%"
let forwardSlash     = terminal      "/"
let singleEquals     = terminal      "="
let doublePipes      = terminal      @"\|\|"
let doubleEquals     = terminal      "=="
let bangEquals       = terminal      "!="
let openAngleEquals  = terminal      "<="
let openAngle        = terminal      "<"
let closeAngleEquals = terminal      ">="
let closeAngle       = terminal      ">"
let doubleAmpersands = terminal      "&&"
let period           = terminal      @"\."

// Precedence

let optionalElsePrecedenceGroup = configurator.LeftAssociative()

configurator.LeftAssociative(downcast elseKeyword.Symbol) |> ignore

configurator.LeftAssociative(downcast singleEquals.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast doublePipes.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast doubleEquals.Symbol,
                             downcast bangEquals.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast openAngleEquals.Symbol,
                             downcast openAngle.Symbol,
                             downcast closeAngleEquals.Symbol,
                             downcast closeAngle.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast doubleAmpersands.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast exclamation.Symbol,
                             downcast plus.Symbol,
                             downcast minus.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast asterisk.Symbol,
                             downcast forwardSlash.Symbol,
                             downcast percent.Symbol)
                             |> ignore

let binaryExpressionPrecedenceGroup = configurator.LeftAssociative()
let unaryExpressionPrecedenceGroup  = configurator.RightAssociative()

// Productions

program.AddProduction(declarationList).SetReduceToFirst()

declarationList.AddProduction(declarationList, declaration).SetReduceFunction (fun a b -> a @ [b])
declarationList.AddProduction(declaration).SetReduceFunction (fun a -> [a])

declaration.AddProduction(staticVariableDeclaration).SetReduceFunction (fun a -> Ast.StaticVariableDeclaration a)
declaration.AddProduction(functionDeclaration)      .SetReduceFunction (fun a -> Ast.FunctionDeclaration a)

typeSpec.AddProduction(voidKeyword).SetReduceFunction (fun _ -> Ast.Void)
typeSpec.AddProduction(boolKeyword).SetReduceToFirst()
typeSpec.AddProduction(intKeyword).SetReduceToFirst()
typeSpec.AddProduction(floatKeyword).SetReduceToFirst()

staticVariableDeclaration.AddProduction(typeSpec, identifier, semicolon)
    .SetReduceFunction (fun a b _ -> Ast.ScalarVariableDeclaration(a, b))
staticVariableDeclaration.AddProduction(typeSpec, identifier, openSquare, closeSquare, semicolon)
    .SetReduceFunction (fun a b _ _ _ -> Ast.ArrayVariableDeclaration(a, b))

functionDeclaration.AddProduction(typeSpec, identifier, openParen, parameters, closeParen, compoundStatement)
    .SetReduceFunction (fun a b _ d _ f -> (a, b, d, f))

parameters.AddProduction(parameterList).SetReduceToFirst()
parameters.AddProduction(voidKeyword).SetReduceFunction (fun _ -> [])

parameterList.AddProduction(parameterList, comma, parameter).SetReduceFunction (fun a _ c -> a @ [c])
parameterList.AddProduction(parameter)                      .SetReduceFunction (fun a -> [a])

parameter.AddProduction(typeSpec, identifier)                         .SetReduceFunction (fun a b -> Ast.ScalarVariableDeclaration(a, b))
parameter.AddProduction(typeSpec, identifier, openSquare, closeSquare).SetReduceFunction (fun a b _ _ -> Ast.ArrayVariableDeclaration(a, b))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction()             .SetReduceFunction (fun () -> [])

statementList.AddProduction(statementList, statement).SetReduceFunction (fun a b -> a @ [b])
statementList.AddProduction(statement)               .SetReduceFunction (fun a -> [a])

statement.AddProduction(expressionStatement).SetReduceFunction (fun a -> Ast.ExpressionStatement a)
statement.AddProduction(compoundStatement)  .SetReduceFunction (fun a -> Ast.CompoundStatement a)
statement.AddProduction(ifStatement)        .SetReduceFunction (fun a -> Ast.IfStatement a)
statement.AddProduction(whileStatement)     .SetReduceFunction (fun a -> Ast.WhileStatement a)
statement.AddProduction(returnStatement)    .SetReduceFunction (fun a -> Ast.ReturnStatement a)
statement.AddProduction(breakStatement)     .SetReduceFunction (fun a -> Ast.BreakStatement)

expressionStatement.AddProduction(expression, semicolon).SetReduceFunction (fun a _ -> Ast.Expression a)
expressionStatement.AddProduction(semicolon)            .SetReduceFunction (fun _ -> Ast.Nop)

whileStatement.AddProduction(whileKeyword, openParen, expression, closeParen, statement)
    .SetReduceFunction (fun a b c d e -> (c, e))

compoundStatement.AddProduction(openCurly, optionalLocalDeclarations, optionalStatementList, closeCurly)
    .SetReduceFunction (fun _ b c _ -> (b, c))

optionalLocalDeclarations.AddProduction(localDeclarations).SetReduceToFirst()
optionalLocalDeclarations.AddProduction()                 .SetReduceFunction (fun () -> [])

localDeclarations.AddProduction(localDeclarations, localDeclaration).SetReduceFunction (fun a b -> a @ [b])
localDeclarations.AddProduction(localDeclaration)                   .SetReduceFunction (fun a -> [a])

localDeclaration.AddProduction(typeSpec, identifier, semicolon)                         .SetReduceFunction (fun a b _ -> Ast.ScalarVariableDeclaration(a, b))
localDeclaration.AddProduction(typeSpec, identifier, openSquare, closeSquare, semicolon).SetReduceFunction (fun a b _ _ _ -> Ast.ArrayVariableDeclaration(a, b))

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

breakStatement.AddProduction(breakKeyword, semicolon).SetReduceFunction (fun _ _ -> ())

expression.AddProduction(identifier, singleEquals, expression)
    .SetReduceFunction (fun a _ c -> Ast.ScalarAssignmentExpression({ Identifier = a }, c))
expression.AddProduction(identifier, openSquare, expression, closeSquare, singleEquals, expression)
    .SetReduceFunction (fun a _ c _ _ f -> Ast.ArrayAssignmentExpression({ Identifier = a }, c, f))

expression.AddProduction(expression, doublePipes, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.ConditionalOr, c))
expression.AddProduction(expression, doubleEquals, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Equal, c))
expression.AddProduction(expression, bangEquals, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.NotEqual, c))
expression.AddProduction(expression, openAngleEquals, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.LessEqual, c))
expression.AddProduction(expression, openAngle, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Less, c))
expression.AddProduction(expression, closeAngleEquals, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.GreaterEqual, c))
expression.AddProduction(expression, closeAngle, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Greater, c))
expression.AddProduction(expression, doubleAmpersands, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.ConditionalAnd, c))
expression.AddProduction(expression, plus, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Add, c))
expression.AddProduction(expression, minus, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Subtract, c))
expression.AddProduction(expression, asterisk, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Multiply, c))
expression.AddProduction(expression, forwardSlash, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Divide, c))
expression.AddProduction(expression, percent, expression).SetReduceFunction (fun a _ c -> Ast.BinaryExpression(a, Ast.Modulus, c))

let unaryExpressionProduction = expression.AddProduction(unaryOperator, expression)
unaryExpressionProduction.SetReduceFunction (fun a b -> Ast.UnaryExpression(a, b))
unaryExpressionProduction.SetPrecedence unaryExpressionPrecedenceGroup

expression.AddProduction(openParen, expression, closeParen).SetReduceFunction (fun _ b _ -> b)

expression.AddProduction(identifier).SetReduceFunction (fun a -> Ast.IdentifierExpression ({ Identifier = a }))
expression.AddProduction(identifier, openSquare, expression, closeSquare)
    .SetReduceFunction (fun a _ c _ -> Ast.ArrayIdentifierExpression({ Identifier = a }, c))
expression.AddProduction(identifier, openParen, optionalArguments, closeParen)
    .SetReduceFunction (fun a _ c _ -> Ast.FunctionCallExpression(a, c))
expression.AddProduction(identifier, period, sizeKeyword)
    .SetReduceFunction (fun a _ _ -> Ast.ArraySizeExpression { Identifier = a })

expression.AddProduction(trueLiteral) .SetReduceFunction (fun a -> Ast.LiteralExpression a)
expression.AddProduction(falseLiteral).SetReduceFunction (fun a -> Ast.LiteralExpression a)
expression.AddProduction(intLiteral)  .SetReduceFunction (fun a -> Ast.LiteralExpression a)
expression.AddProduction(floatLiteral).SetReduceFunction (fun a -> Ast.LiteralExpression a)

expression.AddProduction(newKeyword, typeSpec, openSquare, expression, closeSquare)
    .SetReduceFunction (fun _ b _ d _ -> Ast.ArrayAllocationExpression(b, d))

unaryOperator.AddProduction(exclamation).SetReduceFunction (fun a -> Ast.LogicalNegate)
unaryOperator.AddProduction(minus)      .SetReduceFunction (fun a -> Ast.Negate)
unaryOperator.AddProduction(plus)       .SetReduceFunction (fun a -> Ast.Identity)

optionalArguments.AddProduction(arguments).SetReduceToFirst()
optionalArguments.AddProduction()         .SetReduceFunction (fun () -> [])

arguments.AddProduction(arguments, comma, expression).SetReduceFunction (fun a _ c -> a @ [c])
arguments.AddProduction(expression)                  .SetReduceFunction (fun a -> [a])

// Ignore whitespace and comments
configurator.LexerSettings.Ignore <- [| @"\s+"; @"/\*[^(\*/)]*\*/"; @"//[^\n]*\n" |]



let parser = configurator.CreateParser()

let parse (s : string) =
    try
        parser.Parse(s) :?> Program
    with
        | :? Piglet.Lexer.LexerException as ex ->
            raise (lexerError ex.Message)
        | :? Piglet.Parser.ParseException as ex ->
            raise (parserError ex.Message)