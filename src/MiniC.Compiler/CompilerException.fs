namespace MiniC.Compiler

type CompilerException(message : string) =
    inherit System.Exception(message)