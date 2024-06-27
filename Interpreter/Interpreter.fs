//author: Nicolai Pavliuc
module Interpreter
open TypesForAST
open System

type State = Map<string, float>


let runCommands (commands: CommandS list) (currentState: State) =
    let rec runCommandsHelper (commands: CommandS list) (currentState: State) =
        match commands with
        | [] -> currentState
        | x :: xs ->
            match x with
            | Ld(s1, s2) ->
                let newVal = Map.find s1 currentState
                let newState = Map.add s2 newVal currentState
                runCommandsHelper xs newState
            | Add(s1, s2, s3) ->
                let s1' = Map.find s1 currentState
                let s2' = Map.find s2 currentState
                let newVal = s1' + s2'
                let newState = Map.add s3 newVal currentState
                runCommandsHelper xs newState
            | Sub(s1, s2, s3) ->
                let s1' = Map.find s1 currentState
                let s2' = Map.find s2 currentState
                let newVal = s1' - s2'

                let newState =
                    if newVal < 0.0 then
                        Map.add s3 0.0 currentState
                    else
                        Map.add s3 newVal currentState

                runCommandsHelper xs newState
            | Mul(s1, s2, s3) ->
                let s1' = Map.find s1 currentState
                let s2' = Map.find s2 currentState
                let newVal = s1' * s2'
                let newState = Map.add s3 newVal currentState
                runCommandsHelper xs newState
            | Div(s1, s2, s3) ->
                let s1' = Map.find s1 currentState
                let s2' = Map.find s2 currentState
                let newVal = s1' / s2'
                let newState = Map.add s3 newVal currentState
                runCommandsHelper xs newState
            | Sqrt(s1, s2) ->
                let s1' = Map.find s1 currentState
                let newVal = sqrt s1'
                let newState = Map.add s2 newVal currentState
                runCommandsHelper xs newState
            | Cmp(s1, s2) ->
                let s1' = Map.find s1 currentState
                let s2' = Map.find s2 currentState

                let newVal =
                    if s1' > s2' then 1.0
                    else if s1' < s2' then -1.0
                    else 0.0

                let newState = Map.add "cmp" newVal currentState
                runCommandsHelper xs newState
            | IfGT(commands: CommandS list) ->
                if Map.find "cmp" currentState = 1.0 then
                    let newState = runCommandsHelper commands currentState
                    runCommandsHelper xs newState
                else
                    runCommandsHelper xs currentState
            | IfGE(commands: CommandS list) ->
                if Map.find "cmp" currentState >= 0.0 then
                    let newState = runCommandsHelper commands currentState
                    runCommandsHelper xs newState
                else
                    runCommandsHelper xs currentState
            | IfLT(commands: CommandS list) ->
                if Map.find "cmp" currentState < 0.0 then
                    let newState = runCommandsHelper commands currentState
                    runCommandsHelper xs newState
                else
                    runCommandsHelper xs currentState
            | IfLE(commands: CommandS list) ->
                if Map.find "cmp" currentState <= 0.0 then
                    let newState = runCommandsHelper commands currentState
                    runCommandsHelper xs newState
                else
                    runCommandsHelper xs currentState
            | IfEQ(commands: CommandS list) ->
                if Map.find "cmp" currentState = 0.0 then
                    let newState = runCommandsHelper commands currentState
                    runCommandsHelper xs newState
                else
                    runCommandsHelper xs currentState

    runCommandsHelper commands currentState


let rec runCrn (crn: Crn) (currentState: State) (initializeConcs: bool) (doNotRunSteps: bool) =
    match crn with
    | [] -> currentState
    | Conc(sp, conc) :: xs ->
        if initializeConcs then
            let newState = Map.add sp conc currentState
            runCrn xs newState initializeConcs doNotRunSteps
        else
            runCrn xs currentState initializeConcs doNotRunSteps
    | StepS(commands) :: xs ->
        if not doNotRunSteps then
            let newState = runCommands commands currentState
            runCrn xs newState initializeConcs doNotRunSteps
        else
            currentState //skip steps


let runInterpreter (crn: Crn) (currentState: State) (doNotRunSteps: bool) =
    let initializeConcs = Map.isEmpty currentState
    runCrn crn currentState initializeConcs doNotRunSteps

let runMultipleTimesSeq (crn: Crn) (initialState: State) =
    let initialStateConcsOnly = runInterpreter crn initialState true
    let initialSeq = Seq.singleton initialStateConcsOnly

    let main =
        Seq.init Int32.MaxValue (fun _ -> ())
        |> Seq.scan (fun currentState _ -> runInterpreter crn currentState false) initialState
        |> Seq.tail

    Seq.append initialSeq main