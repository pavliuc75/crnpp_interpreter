//author: Nicolai Pavliuc
module TreeVisialization
open Plotly.NET
open TypesForAST

type Tree<'a> = Node of 'a * Tree<'a> list

let buildTree (crn: Crn) : Tree<string> =
    let rec commandToTree (cmd: CommandS) : Tree<string> =
        match cmd with
        | Ld(s1, s2) -> Node("Load", [ Node(s1, []); Node(s2, []) ])
        | Add(s1, s2, s3) -> Node("Add", [ Node(s1, []); Node(s2, []); Node(s3, []) ])
        | Sub(s1, s2, s3) -> Node("Sub", [ Node(s1, []); Node(s2, []); Node(s3, []) ])
        | Mul(s1, s2, s3) -> Node("Mul", [ Node(s1, []); Node(s2, []); Node(s3, []) ])
        | Div(s1, s2, s3) -> Node("Div", [ Node(s1, []); Node(s2, []); Node(s3, []) ])
        | Sqrt(s1, s2) -> Node("Sqrt", [ Node(s1, []); Node(s2, []) ])
        | Cmp(s1, s2) -> Node("Cmp", [ Node(s1, []); Node(s2, []) ])
        | IfGT cmds -> Node("IfGT", cmds |> List.map commandToTree)
        | IfGE cmds -> Node("IfGE", cmds |> List.map commandToTree)
        | IfLT cmds -> Node("IfLT", cmds |> List.map commandToTree)
        | IfLE cmds -> Node("IfLE", cmds |> List.map commandToTree)
        | IfEQ cmds -> Node("IfEQ", cmds |> List.map commandToTree)

    let nodeForRoot (root: RootS) : Tree<string> =
        match root with
        | Conc(species, amount) -> Node("Conc", [ Node(species, []); Node(sprintf "%f" amount, []) ])
        | StepS commands -> Node("StepS", commands |> List.map commandToTree)

    Node("Crn", crn |> List.map nodeForRoot)
    

//-------------------------------------------------------
//-------------------------------------------------------
//-------------------------------------------------------
//-------------------------------------------------------
//-------------------------------------------------------
//-------------------------------------------------------
//rest is from previous project


    
type Extent = (float * float) list

let movetree (Node ((label, x), subtrees), x': float) = Node((label, x + x'), subtrees)

let moveextent (e: Extent, x) =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec merge ps qs =
    match (ps, qs) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ptail, (_, q) :: qtail) -> (p, q) :: (merge ptail qtail)

let mergelist (es: Extent list) = List.fold merge [] es

let rmax (p: float, q: float) = if p > q then p else q

let rec fit (ps: Extent, qs: Extent) =
    match (ps, qs) with
    | ((_, p) :: ptail, ((q, _) :: qtail)) -> rmax (fit (ptail, qtail), p - q + 1.0)
    | (_, _) -> 0.0

let fitlistl es =
    let rec fitlistl' acc es' =
        match (acc, es') with
        | (_, []) -> []
        | (acc, (e' :: etail)) ->
            let x = fit (acc, e')

            x
            :: fitlistl' (merge acc (moveextent (e', x))) etail

    fitlistl' [] es

let fitlistr es =
    let rec fitlistr' acc es' =
        match (acc, es') with
        | (_, []) -> []
        | (acc, (e' :: etail)) ->
            let x = -1.0 * fit (e', acc)

            x
            :: fitlistr' (merge (moveextent (e', x)) acc) etail

    List.rev (fitlistr' [] (List.rev es))

let mean (x: float, y: float) = (x + y) / 2.0

let fitlist es =
    List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
    let rec design' (Node (label, subtrees)) =
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map movetree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergelist pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)

    design' (tree)

let plot (Node ((l, x: float), tree)) =
    let rec getPlots isroot parentx parenty subtree =
        match subtree with
        | [] -> []
        | (Node ((l', x'), subtree')) :: rest ->
            [ Chart.Line(
                  x = [ parentx; (parentx + x') ],
                  y = [ parenty; parenty - 1.0 ], 
                  MultiText =
                      [ (if isroot then (sprintf "%A" l) else "")
                        (sprintf "%A" l') ],
                  TextPosition = StyleParam.TextPosition.TopRight,
                  ShowMarkers = true
              ) ]
            @ (getPlots false (parentx + x') (parenty - 1.0) subtree')
              @ (getPlots false parentx parenty rest)

    Defaults.DefaultHeight <- 1000
    Defaults.DefaultWidth <- 1000

    let plots = getPlots true x 0.0 tree

    plots |> Chart.combine |> Chart.show
    
let visualize (crn: Crn) =
    let tree = buildTree crn
    let (tree', _) = design tree
    plot tree'    
