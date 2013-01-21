module MiniC.Compiler.ParsingUtilities

open Piglet.Parser
open Piglet.Parser.Configuration

type ProductionWrapperBase (production : IProduction<obj>) =
    member x.Production = production
    member x.SetReduceToFirst () = production.SetReduceToFirst()
    member x.SetPrecedence(precedenceGroup) = production.SetPrecedence(precedenceGroup)

type ProductionWrapper<'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : (unit -> 'T)) =
        production.SetReduceFunction (fun o -> box (f ()))

type ProductionWrapper<'a,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])))

type ProductionWrapper<'a,'b,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0]) (unbox o.[1])))

type ProductionWrapper<'a,'b,'c,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])))

type ProductionWrapper<'a,'b,'c,'d,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])))

type ProductionWrapper<'a,'b,'c,'d,'e,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])))

type ProductionWrapper<'a,'b,'c,'d,'e,'f,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])))

type ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])
                                                      (unbox o.[6])))

type SymbolWrapper<'T> (symbol : ISymbol<obj>) =
    member x.Symbol = symbol

type TerminalWrapper<'T> (terminal : ITerminal<obj>) =
    inherit SymbolWrapper<'T>(terminal)

type NonTerminalWrapper<'T> (nonTerminal : INonTerminal<obj>) =
    inherit SymbolWrapper<'T>(nonTerminal)

    member x.AddProduction () =
        let production = nonTerminal.AddProduction()
        new ProductionWrapper<'T>(production)
    
    member x.AddProduction (part : SymbolWrapper<'a>) =
        let production = nonTerminal.AddProduction(part.Symbol)
        new ProductionWrapper<'a,'T>(production)
    
    member x.AddProduction((part1 : SymbolWrapper<'a>), (part2 : SymbolWrapper<'b>)) =
        let production = nonTerminal.AddProduction(part1.Symbol, part2.Symbol)
        new ProductionWrapper<'a,'b,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol)
        new ProductionWrapper<'a,'b,'c,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>),
                           (part6 : SymbolWrapper<'f>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'f,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>),
                           (part6 : SymbolWrapper<'f>),
                           (part7 : SymbolWrapper<'g>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol,
                                                   part7.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'T>(production)