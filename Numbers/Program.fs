open System
open System.IO
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
    
let rec print (output : TextWriter) = function
    | [] -> ()
    | Const c :: expr -> output.Write c; print output expr
    | Op o :: expr -> output.Write (toString o); print output expr

let monitor = obj ()
let printExpression (o : TextWriter) e =
    lock monitor <| fun () -> 
        o.Write(print o e)
        o.WriteLine()

let combine expr operator operand = List.append expr [ Op operator; Const operand ]    

let rec private gen output expression = function
    | [] -> if execute expression = target then printExpression output expression
    | number :: ns ->
        Array.iter (fun o -> gen output (combine expression o number) ns) operations

let solvePermutation' output = function
    | n :: ns -> gen output [Const n] ns

let solved = ref 0
let solvePermutation output p =
    let count = System.Threading.Interlocked.Increment solved
    solvePermutation' output p
    System.Console.Error.WriteLine ("Permutation {0} finished", count)

[<EntryPoint>]
let rec main = function
    | [| fileName |] ->
        use result = new FileStream(fileName, FileMode.Create, FileAccess.Write)
        use writer = new StreamWriter(result)
        let permutations = List.permutations numbers
        ParallelEnumerable.AsParallel permutations
        |> fun s -> ParallelEnumerable.ForAll (s, Action<_>(solvePermutation writer))
        0
    | _ -> main [| "numbers.txt" |]
