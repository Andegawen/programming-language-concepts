type AExpr =
    | CstI of int
    | Var of string
    | Add of AExpr * AExpr
    | Mul of AExpr * AExpr
    | Sub of AExpr * AExpr

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x

let rec eval aExpr env : int =
    match aExpr with
    | CstI i -> i
    | Var name -> lookup env name
    | Add(a1, a2) -> eval a1 env +  eval a2 env
    | Mul(a1, a2) -> eval a1 env *  eval a2 env
    | Sub(a1, a2) -> eval a1 env -  eval a2 env

let precidence aExpr = 
    match aExpr with
    | CstI _ -> 0
    | Var _ -> 0
    | Add _ -> 1
    | Mul _ -> 2
    | Sub _ -> 1

let symbol aExpr = 
    match aExpr with
    | Add _ -> "+"
    | Mul _ -> "*"
    | Sub _ -> "-"
    | _ -> ""

let rec fmt' (parentPrec:int) aExpr =
    let currentPrec = precidence aExpr
    match aExpr with
    | CstI i -> i.ToString()
    | Var name -> name
    | Add(a1,a2) | Mul(a1,a2) | Sub(a1,a2) -> 
        match currentPrec with
        | x when x < parentPrec ->"(" + fmt' currentPrec a1 + symbol aExpr + fmt' currentPrec a2 + ")"
        | _ -> fmt' currentPrec a1 + symbol aExpr + fmt' currentPrec a2

let fmt aExpr = fmt' 0 aExpr

//0+e => e
//e+0 => e
//0*e => 0
//e*0 => 0
//1*e => e
//e*1 => e
//e-e => 0
//e-0 => e
let rec simplify aExpr =
    match aExpr with
    | Add(a1,a2) ->
        let as1 = simplify a1
        let as2 = simplify a2
        match as1, as2 with
        | (CstI 0, x) -> x
        | (x, CstI 0) -> x
        | _ -> Add(as1,as2)
    | Mul(a1,a2) ->
        let as1 = simplify a1
        let as2 = simplify a2
        match as1, as2 with
        | (CstI 0,_) -> CstI 0
        | (_, CstI 0) -> CstI 0
        | (CstI 1, x) -> x
        | (x, CstI 1) -> x
        | _ -> Mul(as1, as2)
    | Sub(a1,a2) ->
        let as1 = simplify a1
        let as2 = simplify a2
        match as1, as2 with
        | (x, CstI 0) -> x
        | (x,y) -> if x=y then CstI 0 else Sub(x,y)
    | ex -> ex


let rec diff aExpr =
    match aExpr with
    | CstI _ -> CstI 0
    | Var name ->
        match name with
        | "x" -> CstI 1
        | _ -> CstI 0
    | Add(a1, a2) -> Add(diff a1, diff a2)
    | Sub(a1, a2) -> Sub(diff a1, diff a2)
    | Mul(a1, a2) -> Add(Mul(diff a1, a2), Mul(a1, diff a2))

let eq = Mul(Var "x", Add(CstI 3, Var "x"))
let differentialExpression = simplify (diff eq)
printfn "d/dx(%A) = %A" eq differentialExpression


// ((1-1)*13)+25 = 25
let weirdExp = Add(Mul(Sub(CstI 1, CstI 1), CstI 13), CstI 25)
printfn "weird simplified: %A" (simplify weirdExp)

let weirdExp2 = Add(CstI 0, CstI 3)
printfn "weird simplified: %A" (simplify weirdExp2)

let x = simplify (diff weirdExp)

let env = [("x", 3); ("y", 78); ("z", 10); ("v", 666); ("w", 111)]

// r0: x * (y+3)
//For old expression Type: Prim("*", Var "y", CstI 3))
let r0 = Mul(Var "x", Add(Var "y", CstI 3))

//r1: v-(w+z)
let r1 = Sub(Var "v", Add(Var "w", Var "z"))
//r2: 2*(v-(w-z))
let r2 = Mul(CstI 2, Sub(Var "v", Sub(Var "w", Var "z")))
//r3: x+y+z+v
let r3 = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))

let r4 = Add(Var "x", Add(Var "y", Mul(Var "z", Var "v")))
let expressions = [r0;r1;r2;r3;r4]
for aExpr in expressions do
    printfn "expression %A" aExpr
    printfn "   %s=%d" (fmt aExpr) (eval aExpr env)
