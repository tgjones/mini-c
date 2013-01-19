module MiniC.Compiler.Parser

open Piglet.Parser
open Ast

type ProductionWrapperBase (production : Configuration.IProduction<obj>) =
    member x.Production = production
    member x.SetReduceToFirst () = production.SetReduceToFirst()

type ProductionWrapper<'T> (production : Configuration.IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : (unit -> 'T)) =
        production.SetReduceFunction (fun o -> box (f ()))

type ProductionWrapper<'T,'U> (production : Configuration.IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])))

type ProductionWrapper<'T,'U,'V> (production : Configuration.IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0]) (unbox o.[1])))

type ProductionWrapper<'T,'U,'V,'W> (production : Configuration.IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V -> 'W)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])))

type ProductionWrapper<'T,'U,'V,'W,'X,'Y,'Z> (production : Configuration.IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V -> 'W -> 'X -> 'Y -> 'Z)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])))

type SymbolWrapper<'T> (symbol : Configuration.ISymbol<obj>) =
    member x.Symbol = symbol

type TerminalWrapper<'T> (terminal : Configuration.ITerminal<obj>) =
    inherit SymbolWrapper<'T>(terminal)

type NonTerminalWrapper<'T> (nonTerminal : Configuration.INonTerminal<obj>) =
    inherit SymbolWrapper<'T>(nonTerminal)

    member x.AddProduction () =
        let production = nonTerminal.AddProduction()
        new ProductionWrapper<'T>(production)
    
    member x.AddProduction((part : SymbolWrapper<'U>)) =
        let production = nonTerminal.AddProduction(part.Symbol)
        new ProductionWrapper<'U,'T>(production)
    
    member x.AddProduction((part1 : SymbolWrapper<'U>), (part2 : SymbolWrapper<'V>)) =
        let production = nonTerminal.AddProduction(part1.Symbol, part2.Symbol)
        new ProductionWrapper<'U,'V,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'U>),
                           (part2 : SymbolWrapper<'V>),
                           (part3 : SymbolWrapper<'W>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol)
        new ProductionWrapper<'U,'V,'W,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'U>),
                           (part2 : SymbolWrapper<'V>),
                           (part3 : SymbolWrapper<'W>),
                           (part4 : SymbolWrapper<'X>),
                           (part5 : SymbolWrapper<'Y>),
                           (part6 : SymbolWrapper<'Z>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol)
        new ProductionWrapper<'U,'V,'W,'X,'Y,'Z,'T>(production)

let configurator = ParserFactory.Configure<obj>()

// Non-terminals

let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())

let program               = nonTerminal<Program>()
let declarationList       = nonTerminal<Declaration list>()
let declaration           = nonTerminal<Declaration>()
let functionDeclaration   = nonTerminal<Declaration>()
let compoundStatement     = nonTerminal<CompoundStatement>()
let typeSpec              = nonTerminal<TypeSpec>()
let parameters            = nonTerminal<Parameters>()
let optionalStatementList = nonTerminal<Statement list>()
let statementList         = nonTerminal<Statement list>()
let statement             = nonTerminal<Statement>()
let expressionStatement   = nonTerminal<ExpressionStatement>()
let expression            = nonTerminal<Expression>()
let returnStatement       = nonTerminal<Expression option>()

// Terminals

let terminalParse<'T> regex (onParse : (string -> 'T)) =
    new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

let terminal regex =
    new TerminalWrapper<string>(configurator.CreateTerminal(regex))

let returnKeyword = terminal      "return"
let voidKeyword   = terminal      "void"
let plus          = terminalParse @"\+"   (fun s -> s)
let number        = terminalParse @"\d+"  (fun s -> Ast.IntLiteral(int32 s))
let trueLiteral   = terminalParse "true"  (fun s -> Ast.BoolLiteral(true))
let falseLiteral  = terminalParse "false" (fun s -> Ast.BoolLiteral(false))
let intKeyword    = terminalParse "int"   (fun s -> Ast.Int)
let identifier    = terminalParse @"\w+"  (fun s -> s)
let openParen     = terminal      @"\("
let closeParen    = terminal      @"\)"
let openCurly     = terminal      @"\{"
let closeCurly    = terminal      @"\}"
let semicolon     = terminal      ";"

// Productions

program.AddProduction(declarationList).SetReduceToFirst()

declarationList.AddProduction(declarationList, declaration).SetReduceFunction (fun x y -> x @ [y])
declarationList.AddProduction(declaration).SetReduceFunction (fun x -> [x])

declaration.AddProduction(functionDeclaration).SetReduceToFirst()

expression.AddProduction(expression, plus, expression).SetReduceFunction (fun x y z -> Ast.BinaryExpression(x, Ast.Add, z))
expression.AddProduction(number).SetReduceFunction (fun x -> Ast.LiteralExpression x)

functionDeclaration.AddProduction(typeSpec, identifier, openParen, parameters, closeParen, compoundStatement)
    .SetReduceFunction (fun u v w x y z -> Ast.FunctionDeclaration(u, v, None, z))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction().SetReduceFunction (fun () -> [])

statementList.AddProduction(statementList, statement).SetReduceFunction (fun x y -> x @ [y])
statementList.AddProduction(statement)               .SetReduceFunction (fun x -> [x])

statement.AddProduction(expressionStatement).SetReduceFunction (fun x -> Ast.ExpressionStatement x)
statement.AddProduction(returnStatement)    .SetReduceFunction (fun x -> Ast.ReturnStatement x)

expressionStatement.AddProduction(expression, semicolon).SetReduceFunction (fun x y -> Ast.Expression x)
expressionStatement.AddProduction(semicolon)            .SetReduceFunction (fun x -> Ast.Nop)

compoundStatement.AddProduction(openCurly, optionalStatementList, closeCurly)
    .SetReduceFunction (fun x y z -> (None, y))

returnStatement.AddProduction(returnKeyword, expression, semicolon).SetReduceFunction (fun x y z -> Some y)
returnStatement.AddProduction(returnKeyword, semicolon)            .SetReduceFunction (fun x y -> None)

typeSpec.AddProduction(voidKeyword).SetReduceFunction (fun x -> Ast.Void)
typeSpec.AddProduction(intKeyword).SetReduceToFirst()

parameters.AddProduction(voidKeyword).SetReduceFunction (fun o -> [])

configurator.LeftAssociative(downcast plus.Symbol) |> ignore

configurator.LexerSettings.Ignore <- [|@"\s+"|]
let parser = configurator.CreateParser()

let parse (s : string) = parser.Parse(s)