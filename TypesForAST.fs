//author: Nicolai Pavliuc
module TypesForAST

type CommandS =
    | Ld of string * string
    | Add of string * string * string
    | Sub of string * string * string
    | Mul of string * string * string
    | Div of string * string * string
    | Sqrt of string * string
    | Cmp of string * string
    | IfGT of CommandS list
    | IfGE of CommandS list
    | IfLT of CommandS list
    | IfLE of CommandS list
    | IfEQ of CommandS list

type RootS =
    | Conc of string * float
    | StepS of CommandS list

type Crn = RootS list
