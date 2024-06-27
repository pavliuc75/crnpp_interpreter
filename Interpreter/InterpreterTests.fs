//author: Nicolai Pavliuc

module InterpreterTests

open NUnit.Framework
open Interpreter
open Parser

[<TestCase>]
let ``Should correctly interpret for CRN example`` () =
    let sampleCRN =
        "
        crn={
        conc[a, 32.0],
        conc[b, 12.0],
        step[{
            ld[a, atmp],
            ld[b, btmp],
            cmp[a, b]
        }],
        step[{
            ifGT[{ sub[atmp, btmp, a] }],
            ifLT[{ sub[btmp, atmp, b] }]
        }]
        }"
        
    
    let crn = parse sampleCRN
    let initialState = Map.empty
    let finalStates = Interpreter.runMultipleTimesSeq crn initialState
    let finalStatesAsList = finalStates |> Seq.take 20 |> List.ofSeq
    
    //assertions
    let firstState = List.head finalStatesAsList
    let secondState = List.nth finalStatesAsList 1
    let thirdState = List.nth finalStatesAsList 2
    Assert.That(firstState.["a"], Is.EqualTo(32.0))
    Assert.That(firstState.["b"], Is.EqualTo(12.0))
    Assert.That(secondState.["a"], Is.EqualTo(20.0))
    Assert.That(secondState.["b"], Is.EqualTo(12.0))
    Assert.That(thirdState.["a"], Is.EqualTo(8.0))
    Assert.That(thirdState.["b"], Is.EqualTo(12.0))
 
    
[<TestCase>]
let ``Should correctly interpret for common divisor example`` () =
    let sampleCRN =
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
        
    
    let crn = parse sampleCRN
    let initialState = Map.empty
    let finalStates = Interpreter.runMultipleTimesSeq crn initialState
    let finalStatesAsList = finalStates |> Seq.take 10 |> List.ofSeq
    
    //assertions
    let firstState = List.head finalStatesAsList
    let secondState = List.nth finalStatesAsList 1
    let thirdState = List.nth finalStatesAsList 2
    let fourthState = List.nth finalStatesAsList 3
    Assert.That(firstState.["c"], Is.EqualTo(3.0))
    Assert.That(secondState.["c"], Is.EqualTo(2.0))
    Assert.That(thirdState.["c"], Is.EqualTo(1.0))
    Assert.That(fourthState.["c"], Is.EqualTo(0.0))
               
        
