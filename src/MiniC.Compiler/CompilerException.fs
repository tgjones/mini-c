namespace MiniC.Compiler

type CompilerException(message : string) =
    inherit System.Exception(message)

module CompilerErrors =
    let create m = CompilerException m

    let lexerError a                  = create (sprintf "MC001 Lexer error: %s" a)
    let parserError a                 = create (sprintf "MC002 Parser error: %s" a)
    let variableAlreadyDefined a      = create (sprintf "MC003 A variable named '%s' is already defined in this scope" a)
    let cannotConvertType a b         = create (sprintf "MC004 Cannot convert type '%s' to '%s'" a b)
    let operatorCannotBeApplied a b c = create (sprintf "MC005 Operator '%s' cannot be applied to operands of type '%s' and '%s'" a b c)
    let nameDoesNotExist a            = create (sprintf "MC006 The name '%s' does not exist in the current context" a)
    let invalidArguments a b c d      = create (sprintf "MC007 Call to function '%s' has some invalid arguments. Argument %i: Cannot convert from '%s' to '%s'" a b c d)
    let wrongNumberOfArguments a b c  = create (sprintf "MC008 Function '%s' takes %i arguments, but here was given %i" a b c)
    let noEnclosingLoop()             = create          "MC009 No enclosing loop out of which to break"
    let cannotApplyIndexing a         = create (sprintf "MC010 Cannot apply indexing with [] to an expression of type '%s'" a)
    let functionAlreadyDefined a      = create (sprintf "MC011 A function named '%s' is already defined" a)
    let missingEntryPoint()           = create          "MC012 Program does not contain a 'main' method suitable for an entry point"