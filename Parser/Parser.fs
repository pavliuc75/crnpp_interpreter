//author: Nicolai Pavliuc

module Parser

open FParsec
open TypesForAST

let parseIfGT, setParseIfGT = createParserForwardedToRef ()
let parseIfGE, setParseIfGE = createParserForwardedToRef ()
let parseIfLT, setParseIfLT = createParserForwardedToRef ()
let parseIfLE, setParseIfLE = createParserForwardedToRef ()
let parseIfEQ, setParseIfEQ = createParserForwardedToRef ()

//parser
let parseCommandSList: Parser<CommandS list, unit> =
    sepBy
        (((skipString "ld["
           >>. spaces
           >>. pipe2
               (many1Satisfy isLetter .>> skipString "," .>> spaces)
               (many1Satisfy isLetter .>> spaces .>> skipChar ']')
               (fun sp1 sp2 -> Ld(sp1, sp2)))
          <|> (skipString "cmp["
               >>. spaces
               >>. pipe2
                   (many1Satisfy isLetter .>> skipString "," .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 -> Cmp(sp1, sp2)))
          <|> (skipString "add["
               >>. spaces
               >>. pipe3
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 sp3 -> Add(sp1, sp2, sp3)))
          <|> (skipString "sub["
               >>. spaces
               >>. pipe3
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 sp3 -> Sub(sp1, sp2, sp3)))
          <|> (skipString "mul["
               >>. spaces
               >>. pipe3
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 sp3 -> Mul(sp1, sp2, sp3)))
          <|> (skipString "div["
               >>. spaces
               >>. pipe3
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> skipChar ',' .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 sp3 -> Div(sp1, sp2, sp3)))
          <|> (skipString "sqrt["
               >>. spaces
               >>. pipe2
                   (many1Satisfy isLetter .>> skipString "," .>> spaces)
                   (many1Satisfy isLetter .>> spaces .>> skipChar ']')
                   (fun sp1 sp2 -> Sqrt(sp1, sp2)))
          <|> parseIfGT
          <|> parseIfGE
          <|> parseIfLT
          <|> parseIfLE
          <|> parseIfEQ)
         .>> spaces)
        (skipChar ',' >>. spaces)

let parseConditionalCommand
    (commandName: string)
    (constructor: CommandS list -> CommandS)
    : Parser<CommandS, unit> =
    skipString (commandName + "[{") .>> spaces >>. parseCommandSList |>> constructor
    .>> spaces
    .>> skipString "}]"

setParseIfGT.Value <- parseConditionalCommand "ifGT" IfGT
setParseIfGE.Value <- parseConditionalCommand "ifGE" IfGE
setParseIfLT.Value <- parseConditionalCommand "ifLT" IfLT
setParseIfLE.Value <- parseConditionalCommand "ifLE" IfLE
setParseIfEQ.Value <- parseConditionalCommand "ifEQ" IfEQ

let parseRootSList: Parser<RootS list, unit> =
    sepBy
        (((spaces
           >>. skipString "conc["
           >>. spaces
           >>. pipe2
               (many1Satisfy isLetter .>> skipString "," .>> spaces)
               (pfloat .>> spaces .>> skipString "]")
               (fun species concentration -> Conc(species, concentration)))
          <|> (skipString "step[{" >>. spaces >>. parseCommandSList |>> StepS
               .>> spaces
               .>> skipString "}]"))
         .>> spaces)
        (skipString "," >>. spaces)

let parseCrn: Parser<Crn, unit> =
    spaces >>. skipString "crn={" >>. spaces >>. parseRootSList
    .>> spaces
    .>> skipString "}"
    .>> spaces
    .>> eof

let parse str =
    match run parseCrn str with
    | Success(res, st, pos) -> res
    | Failure(str, err, st) -> failwith str
