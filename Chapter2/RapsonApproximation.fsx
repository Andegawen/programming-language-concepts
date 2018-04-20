//get approximation
let next n a_i: float = (a_i+n/a_i)/2.0

//compute approximations indifinietly
let rec repeat (func:float->float) (a0:float) : seq<float>   = 
    let a1 = func a0;
    seq {
        yield a1
        yield! repeat func a1 
     }

//limit sequence to find the result
let rec within (eps:float) (sequence:seq<float>) :float =
    let first = Seq.head sequence;
    let rest = Seq.skip 1 sequence
    let second = Seq.head rest;
    if abs(first-second) <= eps then second else within eps rest

//square root
let magicSqrt (n:float) a0 eps = within eps (repeat (next n) a0)
let Number = 9.0;
magicSqrt Number (Number/2.0) 0.01 |> printfn "%A"



//conventional approach of square root
let rec sqrtStep (x : float) (current : float) (eps: float) : float = 
    let nxt = (next x current)
    if abs(nxt - current) < eps then nxt else (sqrtStep x nxt eps)

let sqrt (x: float) (eps:float) : float = 
    sqrtStep x (x/2.0) eps

sqrt 567.0 0.0001 |> printfn "%A"

repeat 