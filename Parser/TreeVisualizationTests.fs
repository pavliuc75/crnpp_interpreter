//author: Nicolai Pavliuc

module TreeVisualizationTests

open FsCheck.NUnit
open TypesForAST
open TreeVisialization

[<Property>]
let ``Tree root should have same number of children as original CRN`` (crn: Crn) =
        let tree = buildTree crn
        match tree with
        | Node(_, children) -> children.Length = crn.Length
        | _ -> false
