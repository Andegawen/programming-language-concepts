//a(next)= (a_i + n/a_i)/2

let Number = 567.0;
let next n x: float = (x+n/x)/2.0

let rec repeat (a0:float) : seq<float>   = 
    let a1 = next Number a0;
    seq {
        yield a1
        yield! repeat a1 
     }
     

let rec within (eps:float) (sequence:seq<float>) :float =
    match ((Seq.head sequence), Seq.skip 1 sequence) with
    | a, rest when abs(a-Seq.head rest) <= eps -> Seq.head rest
    | _, rest -> within eps rest

let magicSqrt a0 eps = within eps (repeat a0)
magicSqrt 300.0 0.01 |> printfn "%A"    

let rec sqrtStep (x : float) (current : float) (eps: float) : float = 
    let nxt = (next x current)
    if abs(nxt - current) < eps then nxt else (sqrtStep x nxt eps)

let sqrt (x: float) (eps:float) : float = 
    sqrtStep x (x/2.0) eps

sqrt 567.0 0.0001 |> printfn "%A"

repeat 