//author: Nicolai Pavliuc

module SemanticCheckerTests

open NUnit.Framework
open Parser
open SemanticChecker

[<TestCase>]
let ``Should return true if semantically correct`` () =
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
    let result = check crn

    Assert.That(result, Is.True)


[<TestCase>]
let ``Should return false if semantically wrong rule1`` () =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,c],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    let crn = parse testInput
    let result = check crn

    Assert.That(result, Is.False)


[<TestCase>]
let ``Should return false if semantically wrong rule2`` () =
    let testInput =
        "crn={
        conc[c,-3],
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
    let result = check crn

    Assert.That(result, Is.False)


[<TestCase>]
let ``Should return false if semantically wrong rule3`` () =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[zero,one,cnext],
            cmp[cnext,zero]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    let crn = parse testInput
    let result = check crn

    Assert.That(result, Is.False)

[<TestCase>]
let ``Should return false if semantically wrong rule4`` () =
    let testInput =
        "crn={
        conc[c,3],
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,cnext]
        }],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    let crn = parse testInput
    let result = check crn

    Assert.That(result, Is.False)


[<TestCase>]
let ``Should return false if semantically wrong rule5`` () =
    let testInput =
        "crn={
        conc[cInitial,3],
        conc[one,1],
        conc[zero,0],
        step[{
            sub[c,one,cnext],
            cmp[c,zero]
        }],
        conc[c,3],
        step[{
            ifGT[{ ld[cnext,c] }],
            ifLE[{ ld[cInitial,c] }]
        }]
    }"

    let crn = parse testInput
    let result = check crn

    Assert.That(result, Is.False)

