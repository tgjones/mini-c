module MiniC.Compiler.Parser

open FParsec
open Ast

let pIdentifier = identifier (IdentifierOptions())

let pLParen = pstring "("
let pRParen = pstring ")"

let pLCurly = pstring "{"
let pRCurly = pstring "}"

let pSemicolon = pstring ";"

let ws = spaces1

let pCompoundStatement, pCompoundStatementRef = 
    createParserForwardedToRef<CompoundStatement, unit>()

let pTypeSpec = choice [pstring "void"  >>% TypeSpec.Void
                        pstring "bool"  >>% TypeSpec.Bool
                        pstring "int"   >>% TypeSpec.Int
                        pstring "float" >>% TypeSpec.Float]

let pParameters = pstring "void" >>% None

let pExpressionStatement = pSemicolon >>% Nop

let pStatement = choice [pExpressionStatement |>> (fun x -> ExpressionStatement(x))
                         pCompoundStatement   |>> (fun x -> CompoundStatement(x))]

let pStatementList = many pStatement

do pCompoundStatementRef := ((ws >>. pLCurly >>. ws) >>. pStatementList .>> pRCurly)
                            |>> (fun x -> (None, x))

let pFunctionDeclaration = pipe4 (pTypeSpec .>> ws) pIdentifier 
                                 (pLParen >>. pParameters .>> pRParen) pCompoundStatement
                                 (fun x y z w -> FunctionDeclaration(x, y, z, w))

let pDeclaration = choice [pFunctionDeclaration]

let pProgram = (many1 pDeclaration) .>> eof

let parse s =
    let result = run pProgram s
    match result with
    | Success(result, _, _) ->
        result
    | Failure(errorAsString, _, _) ->
        failwith errorAsString