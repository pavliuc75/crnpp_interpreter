//author: Nicolai Pavliuc

module SemanticChecker

open TypesForAST

let isRule1Ok (crn: Crn) =
    let rec isRule1OkHelper (crn: Crn) =
        match crn with
        | [] -> true
        | x :: xs ->
            match x with
            | Conc(_, _) -> isRule1OkHelper xs
            | StepS(commands) ->
                let rec isRule1OkHelper2 (commands: CommandS list) =
                    match commands with
                    | [] -> true
                    | x :: xs ->
                        match x with
                        | Ld(s1, s2) -> s1 <> s2 && isRule1OkHelper2 xs
                        | Add(s1, s2, s3) -> s1 <> s3 && s2 <> s3 && isRule1OkHelper2 xs
                        | Sub(s1, s2, s3) -> s1 <> s3 && s2 <> s3 && isRule1OkHelper2 xs
                        | Mul(s1, s2, s3) -> s1 <> s3 && s2 <> s3 && isRule1OkHelper2 xs
                        | Div(s1, s2, s3) -> s1 <> s3 && s2 <> s3 && isRule1OkHelper2 xs
                        | Sqrt(s1, s2) -> s1 <> s2 && isRule1OkHelper2 xs
                        | Cmp(s1, s2) -> s1 <> s2 && isRule1OkHelper2 xs
                        | IfGT(commands) -> isRule1OkHelper2 commands && isRule1OkHelper2 xs
                        | IfGE(commands) -> isRule1OkHelper2 commands && isRule1OkHelper2 xs
                        | IfLT(commands) -> isRule1OkHelper2 commands && isRule1OkHelper2 xs
                        | IfLE(commands) -> isRule1OkHelper2 commands && isRule1OkHelper2 xs
                        | IfEQ(commands) -> isRule1OkHelper2 commands && isRule1OkHelper2 xs

                isRule1OkHelper2 commands && isRule1OkHelper xs

    isRule1OkHelper crn

let isRule2Ok (crn: Crn) =
    let rec isRule2OkHelper (crn: Crn) =
        match crn with
        | [] -> true
        | x :: xs ->
            match x with
            | Conc(_, c) -> c >= 0.0 && isRule2OkHelper xs
            | StepS(_) -> isRule2OkHelper xs

    isRule2OkHelper crn



let isRule3Ok (crn: Crn) =
    let rec hasCycle (graph: Map<string, string list>) visited stack node =
        if Set.contains node stack then
            true
        elif Set.contains node visited then
            false
        else
            let newVisited = Set.add node visited
            let newStack = Set.add node stack

            match Map.tryFind node graph with
            | Some neighbors -> neighbors |> List.exists (hasCycle graph newVisited newStack)
            | None -> false

    let rec buildGraph commands graph =
        match commands with
        | [] -> graph
        | command :: rest ->
            let addEdges inp out graph =
                let current = Map.tryFind out graph |> Option.defaultValue []
                Map.add out (inp :: current) graph

            match command with
            | Ld(inp, out) -> buildGraph rest (addEdges inp out graph)
            | Add(inp1, inp2, out) -> buildGraph rest (addEdges inp1 out (addEdges inp2 out graph))
            | Sub(inp1, inp2, out) -> buildGraph rest (addEdges inp1 out (addEdges inp2 out graph))
            | Mul(inp1, inp2, out) -> buildGraph rest (addEdges inp1 out (addEdges inp2 out graph))
            | Div(inp1, inp2, out) -> buildGraph rest (addEdges inp1 out (addEdges inp2 out graph))
            | Sqrt(inp, out) -> buildGraph rest (addEdges inp out graph)
            | Cmp(inp, out) -> buildGraph rest (addEdges inp out graph)
            | IfGT cmds
            | IfGE cmds
            | IfLT cmds
            | IfLE cmds
            | IfEQ cmds -> buildGraph rest (buildGraph cmds graph)

    let isNoCyclesInModule (commands: CommandS list) =
        let graph = buildGraph commands Map.empty
        let visited = Set.empty
        let stack = Set.empty
        Map.fold (fun acc node _ -> acc || hasCycle graph visited stack node) false graph

    let rec isRule3OkHelper crn =
        match crn with
        | [] -> true
        | Conc(_, _) :: xs -> isRule3OkHelper xs
        | StepS(commands) :: xs -> not (isNoCyclesInModule commands) && isRule3OkHelper xs

    isRule3OkHelper crn


let isRule4Ok (crn: Crn) =
    let rec containsCmp commands =
        match commands with
        | [] -> false
        | x :: xs ->
            match x with
            | Cmp(_, _) -> true
            | _ -> containsCmp xs

    let rec containsConditional commands =
        match commands with
        | [] -> false
        | x :: xs ->
            match x with
            | IfGT _
            | IfGE _
            | IfLT _
            | IfLE _
            | IfEQ _ -> true
            | _ -> containsConditional xs

    let rec checkStepList crn hasSeenCmp =
        match crn with
        | [] -> true
        | StepS(commands) :: xs ->
            let hasCmp = containsCmp commands || hasSeenCmp
            let needsCmp = containsConditional commands

            if needsCmp && not hasCmp then
                false
            else
                checkStepList xs hasCmp
        | _ :: xs -> checkStepList xs hasSeenCmp

    checkStepList crn false

let isRule5Ok (crn: Crn) =
    let rec helper crn seenStep =
        match crn with
        | [] -> true
        | Conc(_, _) :: xs -> if seenStep then false else helper xs seenStep
        | StepS(_) :: xs -> helper xs true

    helper crn false

let check (crn: Crn) =
    //Rule 1: input <> output
    //Rule 2: no negative concentrations
    //Rule 3: no cyclic dependencies in modules
    //Rule 4: if a step contains a conditional, one of previous steps must have a cmp
    //Rule 5: conc should be before steps
    isRule1Ok crn
    && isRule2Ok crn
    && isRule3Ok crn
    && isRule4Ok crn
    && isRule5Ok crn
