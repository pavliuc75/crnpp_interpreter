//author: Nicolai Pavliuc

module Visualization

open XPlot.Plotly

let visualizeGSD = 
    let sampleCRN = "
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
     
    let crn = Parser.parse sampleCRN
    let initialState = Map.empty
    let finalStates = Interpreter.runMultipleTimesSeq crn initialState
    let finalStatesAsList = finalStates |> Seq.take 20 |> List.ofSeq 
     
    let valuesA = List.map (fun m -> m |> Map.find "a") finalStatesAsList
    let valuesB = List.map (fun m -> m |> Map.find "b") finalStatesAsList
    let indices = [0 .. (List.length finalStatesAsList) - 1]
    
    let traceA = Scatter(
        x = indices,
        y = valuesA,
        mode = "lines+markers",
        name = "Value of a"
    )
    
    let traceB = Scatter(
        x = indices,
        y = valuesB,
        mode = "lines+markers",
        name = "Value of b"
    )
    
    let layout = Layout(title = "Values of 'a' and 'b' over iterations",
                        xaxis = Xaxis(title = "Index"),
                        yaxis = Yaxis(title = "Value"))
    
    let data = [traceA; traceB]
    let plot = Chart.Plot(data, layout)
    plot.Show()
    
    
let visualizeDiscreteCounter =
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
                
                
    let crn = Parser.parse sampleCRN
    let initialState = Map.empty
    let finalStates = Interpreter.runMultipleTimesSeq crn initialState
    let finalStatesAsList = finalStates |> Seq.take 10 |> List.ofSeq 

    let valuesC = List.map (fun m -> m |> Map.find "c") finalStatesAsList
    let indices = [0 .. (List.length finalStatesAsList) - 1]
    
    let traceC = Scatter(
        x = indices,
        y = valuesC,
        mode = "lines+markers",
        name = "Value of c"
    )
    
    let layout = Layout(title = "Values of 'c' over iterations",
                        xaxis = Xaxis(title = "Index"),
                        yaxis = Yaxis(title = "Value"))
    
    let data = [traceC;]
    let plot = Chart.Plot(data, layout)
    plot.Show()  