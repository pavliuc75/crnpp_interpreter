//author: Nicolai Pavliuc

[<EntryPoint>]
let main argv =
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

    //parser--------------------------------------------------------------------
    let crn = Parser.parse sampleCRN
    printfn "%A" crn

    //semantic checker-----------------------------------------------------------
    let semainticCheckResult = SemanticChecker.check crn
    printfn "%A" semainticCheckResult

    //interpreter---------------------------------------------------------------
    let initialState = Map.empty
    let finalStates = Interpreter.runMultipleTimesSeq crn initialState
    let finalStatesAsList = finalStates |> Seq.take 20 |> List.ofSeq
    printfn "%A" (finalStatesAsList)

    //visualization of plots--------------------------------------------------------------
    // Visualization.visualizeGSD
    // Visualization.visualizeDiscreteCounter

    //visualization of tree (after parser)--------------------------------------------------------------
    let myCrn: TypesForAST.Crn =
        [ TypesForAST.Conc("a", 32.0)
          TypesForAST.Conc("b", 12.0)
          TypesForAST.StepS
              [ TypesForAST.Ld("a", "atmp")
                TypesForAST.Ld("b", "btmp")
                TypesForAST.Cmp("a", "b") ]
          TypesForAST.StepS
              [ TypesForAST.IfGT [ TypesForAST.Sub("atmp", "btmp", "a") ]
                TypesForAST.IfLT [ TypesForAST.Sub("btmp", "atmp", "b") ] ] ]
    // TreeVisialization.visualize myCrn    

    
    0
