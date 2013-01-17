module MiniC.Compiler.Parser

open FParsec
open Ast

let pIdentifier = (identifier (IdentifierOptions())) .>> spaces

let pLParen = skipString "(" .>> spaces
let pRParen = skipString ")" .>> spaces

let pLCurly = skipString "{" .>> spaces
let pRCurly = skipString "}" .>> spaces

let pSemicolon = skipString ";" .>> spaces

let ws = spaces1

let pCompoundStatement, pCompoundStatementRef = 
    createParserForwardedToRef<CompoundStatement, unit>()

let pBinaryOperator = (choice[skipString "+" >>% BinaryOperator.Add
                              skipString "+" >>% BinaryOperator.Subtract]
                       .>> spaces)

let pTypeSpec = (choice [skipString "void"  >>% TypeSpec.Void
                         skipString "bool"  >>% TypeSpec.Bool
                         skipString "int"   >>% TypeSpec.Int
                         skipString "float" >>% TypeSpec.Float]
                 .>> ws)

let pParameters = ((skipString "void") .>> spaces) >>% None

let pExpression, pExpressionRef =
    createParserForwardedToRef<Expression, unit>()

let pbool =     (stringReturn "true" true)
            <|> (stringReturn "false" false)

let pExpressionP = choice[pbool |>> (fun x -> LiteralExpression(BoolLiteral(x)))
                          pint32 |>> (fun x -> LiteralExpression(IntLiteral(x)))]

do pExpressionRef := pipe3 pExpressionP pBinaryOperator pExpressionP
                           (fun x y z -> BinaryExpression(x, y, z))

let pExpressionStatement = choice [pSemicolon >>% Nop
                                   (pExpression .>> pSemicolon) |>> Expression]

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