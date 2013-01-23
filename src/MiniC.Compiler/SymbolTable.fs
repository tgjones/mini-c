namespace MiniC.Compiler

open System.Collections.Generic
open Ast

type SymbolTable = Dictionary<Expression, IDeclaration>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SymbolTable =
    let create program =
        new SymbolTable()

    let findDeclaration expression symbolTable =
        None