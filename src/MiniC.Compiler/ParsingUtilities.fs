module MiniC.Compiler.ParsingUtilities

open Piglet.Parser
open Piglet.Parser.Configuration

type ProductionWrapperBase (production : IProduction<obj>) =
    member x.Production = production
    member x.SetReduceToFirst () = production.SetReduceToFirst()

type ProductionWrapper<'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : (unit -> 'T)) =
        production.SetReduceFunction (fun o -> box (f ()))

type ProductionWrapper<'T,'U> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])))

type ProductionWrapper<'T,'U,'V> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0]) (unbox o.[1])))

type ProductionWrapper<'T,'U,'V,'W> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V -> 'W)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])))

type ProductionWrapper<'T,'U,'V,'W,'X,'Y> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V -> 'W -> 'X -> 'Y)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])))

type ProductionWrapper<'T,'U,'V,'W,'X,'Y,'Z> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('T -> 'U -> 'V -> 'W -> 'X -> 'Y -> 'Z)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])))

type SymbolWrapper<'T> (symbol : ISymbol<obj>) =
    member x.Symbol = symbol

type TerminalWrapper<'T> (terminal : ITerminal<obj>) =
    inherit SymbolWrapper<'T>(terminal)

type NonTerminalWrapper<'T> (nonTerminal : INonTerminal<obj>) =
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
                           (part5 : SymbolWrapper<'Y>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol)
        new ProductionWrapper<'U,'V,'W,'X,'Y,'T>(production)

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