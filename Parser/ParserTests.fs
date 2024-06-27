//author: Nicolai Pavliuc
module ParserTests

open NUnit.Framework
open Parser

[<TestCase>]
let parsesIfSyntaxIsCorrect () =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,cnext],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    Assert.DoesNotThrow(fun () ->
        let _ = parse testInput
        ()
    )

[<TestCase>]
let failsToParseIfSyntaxIsWrong () =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[
            sub[c,one,cnext],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    Assert.Throws<System.Exception>
        (fun () -> parse testInput|> ignore)
    |> ignore
    
    
[<TestCase>]
let parsesCorrectly() =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,cnext],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"
    
    let crn = parse testInput
    Assert.That(List.length crn, Is.EqualTo(6))
    
    
[<TestCase>]
let parsesCorrectly2() =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,cnext],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"
    
    let crn = parse testInput
    Assert.That(List.head crn, Is.EqualTo(TypesForAST.Conc("c", 3.0)))
    Assert.That(List.head (List.tail crn), Is.EqualTo(TypesForAST.Conc("cInitial", 3.0)))
    Assert.That(List.head (List.tail (List.tail crn)), Is.EqualTo(TypesForAST.Conc("one", 1.0)))
    Assert.That(List.head (List.tail (List.tail (List.tail crn))), Is.EqualTo(TypesForAST.Conc("zero", 0.0)))
    Assert.That(List.head (List.tail (List.tail (List.tail (List.tail crn)))), Is.EqualTo(TypesForAST.StepS [TypesForAST.Sub("c", "one", "cnext"); TypesForAST.Cmp("c", "zero")]))
