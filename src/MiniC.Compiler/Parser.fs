module MiniC.Compiler.Parser

open FParsec
open Ast

let pIdentifier = (identifier (IdentifierOptions())) .>> spaces

let pLParen = skipString "(" .>> spaces
let pRParen = skipString ")" .>> spaces

let pLCurly = pstring "{" .>> spaces
let pRCurly = pstring "}" .>> spaces

let pSemicolon = pstring ";"

let ws = spaces1

let pCompoundStatement, pCompoundStatementRef = 
    createParserForwardedToRef<CompoundStatement, unit>()

let pTypeSpec = (choice [pstring "void"  >>% TypeSpec.Void
                         pstring "bool"  >>% TypeSpec.Bool
                         pstring "int"   >>% TypeSpec.Int
                         pstring "float" >>% TypeSpec.Float]
                 .>> ws)

let pParameters = ((skipString "void") .>> spaces) >>% None

let pExpressionStatement = pSemicolon >>% Nop

let pStatement = choice [pExpressionStatement |>> (fun x -> ExpressionStatement(x))
                         pCompoundStatement   |>> (fun x -> CompoundStatement(x))]

let pStatementList = many pStatement

do pCompoundStatementRef := (pLCurly >>. pStatementList .>> pRCurly)
                            |>> (fun x -> (None, x))

let pFunctionDeclaration = pipe4 pTypeSpec pIdentifier 
                                 (pLParen >>. pParameters .>> pRParen) pCompoundStatement
                                 (fun x y z w -> FunctionDeclaration(x, y, z, w))

let pDeclaration = choice [pFunctionDeclaration]

let pProgram = (spaces >>. (many1 pDeclaration)) .>> eof

let parse s =
    let result = run pProgram s
    match result with
    | Success(result, _, _) ->
        result
    | Failure(errorAsString, _, _) ->
        failwith errorAsString