//---   Countdown example from chapter 11 of Programming in Haskell,
//---   Graham Hutton, Cambridge University Press, 2007.
//---
//---   Transpiled to F# by Simon George as part of FP101x

module CountdownProblem

//---   Expressions

type Op = 
    | Add
    | Sub
    | Mul
    | Div

// valid :: Op -> Int -> Int -> Bool
let valid op x y =
    match op with
        | Add -> true
        | Sub -> x > y
        | Mul -> true
        | Div -> x % y = 0

// apply :: Op -> Int -> Int -> Int
let apply op x y =
    match op with
        | Add -> x + y
        | Sub -> x - y
        | Mul -> x * y
        | Div -> x / y

type Expr =
    | Val of int
    | App of Op * Expr * Expr

// values :: Expr -> [Int]
let rec values expr =
    match expr with
    | Val n         -> [n]
    | App (_, l, r) -> values l @ values r

// eval :: Expr -> [Int]
let rec eval expr =
    match expr with
    | Val n         -> if n > 0 then [n] else []
    | App (o, l, r) -> [ for x in eval l do
                         for y in eval r do
                         if valid o x y then yield apply o x y ]


//---   Combinatorial functions

// subs :: [a] -> [[a]]
let rec subs l = 
    match l with
    | []      -> [[]]
    | (x::xs) -> let yss = subs xs in yss @ List.map (fun y -> x::y) yss

// interleave :: a -> [a] -> [[a]]
let rec interleave x l =
    match l with 
    | []      -> [[x]]
    | (y::ys) -> (x::y::ys) :: List.map (fun x -> y::x) (interleave x ys)

// perms :: [a] -> [[a]]
let rec perms l = 
    match l with
    | []      -> [[]]
    | (x::xs) -> List.concat (List.map (interleave x) (perms xs))

// choices :: [a] -> [[a]]
let choices xs = [ for ys in subs xs do for zs in perms ys -> zs ]
    

//---   Formalising the problem

// solution :: Expr -> [Int] -> Int -> Bool
let solution e ns n = List.contains (values e) (choices ns) && eval e = [n]


//---   Brute force solution

// split :: [a] -> [([a],[a])]
let rec split l =
    match l with
    | []      -> []
    | [_]     -> []
    | (x::xs) -> ([x], xs) :: [ for (ls,rs) in split xs -> (x::ls, rs) ]

// ops :: [Op]
let ops = [ Add ; Sub ; Mul ; Div ]

// combine :: Expr -> Expr -> [Expr]
let combine l r = [ for o in ops -> App (o,l,r) ]

// exprs :: [Int] -> [Expr]
let rec exprs l =
    match l with
    | []  -> []
    | [n] -> [Val n]
    | ns  -> [ for (ls,rs) in split ns do 
               for l in exprs ls do
               for r in exprs rs do
               for e in combine l r do
               yield e ]

// solutions :: [Int] -> Int -> [Expr]
let solutions ns n = [ for ns' in choices ns do
                       for e in exprs ns' do
                       yield eval e = [n] ]


//---   Combining generation and evaluation

type Result = Expr * int

// combine' :: Result -> Result -> [Result]
let combine' (l,x) (r,y) = [ for o in ops do 
                             if valid o x y then yield (App (o,l,r) , apply o x y) ]

// results :: [Int] -> [Result]
let rec results l =
    match l with
    | []  -> []
    | [n] -> [Val n,n]
    | ns  -> [ for (ls,rs) in split ns do 
               for lx in results ls do
               for ry in results rs do
               for res in combine' lx ry do
               yield res ]

// solutions' :: [Int] -> Int -> [Expr]
let solutions' ns n = [ for ns' in choices ns do
                        for (e,m) in results ns' do
                        if m = n then yield e ]


//---   Exploiting numeric properties

// valid' :: Op -> Int -> Int -> Bool
let valid' op x y =
    match op with
        | Add -> x <= y
        | Sub -> x > y
        | Mul -> x <> 1 && y <> 1 && x <= y
        | Div -> (y <> 0) && (y <> 1) && x % y = 0

// combine'' :: Result -> Result -> [Result]
let combine'' (l,x) (r,y) = [ for o in ops do 
                              if valid' o x y then yield (App (o,l,r) , apply o x y) ]

// results' :: [Int] -> [Result]
let rec results' l =
    match l with
    | []  -> []
    | [n] -> [Val n,n]
    | ns  -> [ for (ls,rs) in split ns do 
               for lx in results' ls do
               for ry in results' rs do
               for res in combine'' lx ry do
               yield res ]

// solutions'' :: [Int] -> Int -> [Expr]
let solutions'' ns n = [ for ns' in choices ns do
                         for (e,m) in results' ns' do
                         if m = n then yield e ]


//---   Pretty printing

let showOp o =
    match o with
    | Add -> " + "
    | Sub -> " - "
    | Mul -> " * "
    | Div -> " / "

let rec showExpr e =
    match e with
    | Val n       -> n.ToString()
    | App (o,l,r) -> let bracket x = match x with
                                     | Val y -> y.ToString()
                                     | e     -> "(" + showExpr e + ")"
                     in bracket l + showOp o + bracket r