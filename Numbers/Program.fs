open System
open System.Linq

type Operation =
    | Add
    | Mul
    | Sub
    | Con

let toString = function
    | Add -> "+"
    | Mul -> "*"
    | Sub -> "-"
    | Con -> ""

type ExpressionNode =
    | Const of int
    | Op of Operation

type Expression = ExpressionNode list
    

let operations = [| Add; Mul; Sub; Con |]
let numbers = Enumerable.Range (1, 9) |> Seq.toList
let target = 100

let priorities = [| 3; 2; 1 |]

let priority = function
    | Add -> 1
    | Mul -> 2
    | Sub -> 1
    | Con -> 3

let collapse a b = function
    | Add -> a + b
    | Mul -> a * b
    | Sub -> a - b
    | Con -> a * 10 + b

let rec collapsePriority expr prior =
    match expr with
    | [] | [Const _] -> expr
    | Const a :: Op o :: Const b :: expr' ->
        if priority o = prior then
            collapsePriority ((Const <| collapse a b o) :: expr') prior
        else
            List.append [Const a; Op o] <| collapsePriority (Const b :: expr') prior             

let execute expression =
    let expr = Array.fold collapsePriority expression priorities
    match expr with
    | [Const c] -> c
    | any -> failwithf "Unsolvable expression %A" any
    
let rec print = function
    | [] -> ()
    | Const c :: expr -> printf "%d" c; print expr
    | Op o :: expr -> printf "%s" <| toString o; print expr

let monitor = obj ()
let printExpression e =
    lock monitor <| fun () -> 
        print e
        printfn ""

let combine expr operator operand = List.append expr [ Op operator; Const operand ]    

let rec private gen expression = function
    | [] -> if execute expression = target then printExpression expression
    | number :: ns ->
        Array.iter (fun o -> gen (combine expression o number) ns) operations

let solvePermutation' = function
    | n :: ns -> gen [Const n] ns

let solved = ref 0
let solvePermutation p =
    let count = System.Threading.Interlocked.Increment solved
    solvePermutation' p
    System.Console.Error.WriteLine ("Permutation {0} finished", count)

[<EntryPoint>]
let main _ = 
    let permutations = List.permutations numbers
    ParallelEnumerable.AsParallel permutations
    |> fun s -> ParallelEnumerable.ForAll (s, Action<_>(solvePermutation))
    0
