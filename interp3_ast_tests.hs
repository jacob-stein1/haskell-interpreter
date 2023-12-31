data UOpr = Neg | Not deriving (Show)
data BOpr = Add | Sub | Mul | Div | Mod | And | Or | Lt | Gt | Lte | Gte | Eq deriving (Show)
data Expr
  = Int Int
  | Bool Bool
  | Unit
  | UOpr UOpr Expr
  | BOpr BOpr Expr Expr
  | Var String
  | Fun String String Expr
  | App Expr Expr
  | Let String Expr Expr
  | Seq Expr Expr
  | Ifte Expr Expr Expr
  | Trace Expr
  deriving (Show)

main :: IO ()
main = return ()

{- 

TEST 1 HIGH LEVEL:

trace 1; trace 2

TEST 1 OCAML LOW LEVEL

Seq (Trace (Int 1), Trace (Int 2))

-}

{- 

TEST 2 HIGH LEVEL

let rec fact x =
    if x <= 0 then 1
    else x * fact (x - 1)
in trace (fact 10)

TEST 2 LOW LEVEL

Let ("vfacti1",
   Fun ("vfacti2", "vxi3",
    Ifte (BOpr (Lte, Var "vxi3", Int 0), Int 1,
     BOpr (Mul, Var "vxi3",
      App (Var "vfacti2", BOpr (Sub, Var "vxi3", Int 1))))),
   Trace (App (Var "vfacti1", Int 10)))

-}

{- 

TEST 3 HIGH LEVEL

let fibo x =
    let rec loop i a b =
        trace a;
        if i < x then
            loop (i + 1) b (a + b) 
        else a
    in loop 0 0 1
in trace (fibo 10)

TEST 3 LOW LEVEL

Let ("vfiboi1",
   Fun ("vi2", "vxi3",
    Let ("vloopi4",
     Fun ("vloopi5", "vii6",
      Fun ("vi7", "vai8",
       Fun ("vi9", "vbi10",
        Seq (Trace (Var "vai8"),
         Ifte (BOpr (Lt, Var "vii6", Var "vxi3"),
          App
           (App (App (Var "vloopi5", BOpr (Add, Var "vii6", Int 1)),
             Var "vbi10"),
           BOpr (Add, Var "vai8", Var "vbi10")),
          Var "vai8"))))),
     App (App (App (Var "vloopi4", Int 0), Int 0), Int 1))),
   Trace (App (Var "vfiboi1", Int 10)))

-}

{- 

TEST 4 HIGH LEVEL

let eff x = trace x in
let foo x y z = () in
foo (eff 1) (eff 2) (eff 3)

TEST 4 LOW LEVEL

Let ("veffi1", Fun ("vi2", "vxi3", Trace (Var "vxi3")),
   Let ("vfooi4",
    Fun ("vi5", "vxi6", Fun ("vi7", "vyi8", Fun ("vi9", "vzi10", Unit))),
    App
     (App (App (Var "vfooi4", App (Var "veffi1", Int 1)),
       App (Var "veffi1", Int 2)),
     App (Var "veffi1", Int 3))))

-}

{- 

TEST 5 HIGH LEVEL

let rec mccarthy n =
    if n > 100 then n - 10
    else mccarthy (mccarthy (n + 11))
in
trace (mccarthy 22)

TEST 5 LOW LEVEL

Let ("vmccarthyi1",
   Fun ("vmccarthyi2", "vni3",
    Ifte (BOpr (Gt, Var "vni3", Int 100), BOpr (Sub, Var "vni3", Int 10),
     App (Var "vmccarthyi2",
      App (Var "vmccarthyi2", BOpr (Add, Var "vni3", Int 11))))),
   Trace (App (Var "vmccarthyi1", Int 22)))

-}

{- 

TEST 6 HIGH LEVEL

let rec iter n f g =
    if n <= 0 then g 0
    else f (iter (n - 1) f g)
in
let rec pow x = trace x; x * x in iter 4 pow (fun _ -> 2)

TEST 6 LOW LEVEL

Let ("viteri1",
   Fun ("viteri2", "vni3",
    Fun ("vi4", "vfi5",
     Fun ("vi6", "vgi7",
      Ifte (BOpr (Lte, Var "vni3", Int 0), App (Var "vgi7", Int 0),
       App (Var "vfi5",
        App
         (App (App (Var "viteri2", BOpr (Sub, Var "vni3", Int 1)),
           Var "vfi5"),
         Var "vgi7")))))),
   Let ("vpowi8",
    Fun ("vpowi9", "vxi10",
     Seq (Trace (Var "vxi10"), BOpr (Mul, Var "vxi10", Var "vxi10"))),
    App (App (App (Var "viteri1", Int 4), Var "vpowi8"),
     Fun ("vi11", "vi12", Int 2))))

-}

{- 

TEST 7 HIGH LEVEL

let rec gcd a b =
    if a = 0 then b
    else gcd (b mod a) a
in
trace (gcd 77 11);
trace (gcd 77 121);
trace (gcd 39 91)

TEST 7 LOW LEVEL

Let ("vgcdi1",
   Fun ("vgcdi2", "vai3",
    Fun ("vi4", "vbi5",
     Ifte (BOpr (Eq, Var "vai3", Int 0), Var "vbi5",
      App (App (Var "vgcdi2", BOpr (Mod, Var "vbi5", Var "vai3")),
       Var "vai3")))),
   Seq (Trace (App (App (Var "vgcdi1", Int 77), Int 11)),
    Seq (Trace (App (App (Var "vgcdi1", Int 77), Int 121)),
     Trace (App (App (Var "vgcdi1", Int 39), Int 91)))))

-}

{- 

TEST 8 HIGH LEVEL

let rec bsearch n i j = 
    let k=(i+j)/2 in 
    if i > j then k
    else
        let sq = k * k in 
        if sq = n then k 
        else
            if n > sq
            then bsearch n (k + 1) j 
            else bsearch n i (k - 1)
in
let rec sqrt n = bsearch n 0 n in 
let x = 1234 * 1234 in
trace x;
trace (sqrt x)

TEST 8 LOW LEVEL

Let ("vbsearchi1",
   Fun ("vbsearchi2", "vni3",
    Fun ("vi4", "vii5",
     Fun ("vi6", "vji7",
      Let ("vki8", BOpr (Div, BOpr (Add, Var "vii5", Var "vji7"), Int 2),
       Ifte (BOpr (Gt, Var "vii5", Var "vji7"), Var "vki8",
        Let ("vsqi9", BOpr (Mul, Var "vki8", Var "vki8"),
         Ifte (BOpr (Eq, Var "vsqi9", Var "vni3"), Var "vki8",
          Ifte (BOpr (Gt, Var "vni3", Var "vsqi9"),
           App
            (App (App (Var "vbsearchi2", Var "vni3"),
              BOpr (Add, Var "vki8", Int 1)),
            Var "vji7"),
           App (App (App (Var "vbsearchi2", Var "vni3"), Var "vii5"),
            BOpr (Sub, Var "vki8", Int 1)))))))))),
   Let ("vsqrti10",
    Fun ("vsqrti11", "vni12",
     App (App (App (Var "vbsearchi1", Var "vni12"), Int 0), Var "vni12")),
    Let ("vxi13", BOpr (Mul, Int 1234, Int 1234),
     Seq (Trace (Var "vxi13"), Trace (App (Var "vsqrti10", Var "vxi13"))))))

-}

{- 

TEST 9 HIGH LEVEL

let rec pi n =
    let q = 1 in
    let r = 180 in
    let t = 60 in
    let j = 2 in
    let rec loop n q r t j =
        if n > 0 then
            let u = 3 * (3 * j + 1) * (3 * j + 2) in
            let y = (q * (27 * j - 12) + 5 * r) / (5 * t) in trace y;
            let q' = 10 * q * j * (2 * j - 1) in
            let r' = 10 * u * (q * (5 * j - 2) + r - y * t) in let t' = t * u in
            let j' = j + 1 in
            loop (n - 1) q' r' t' j'
        else () 
    in
    loop n q r t j
in
pi 6

TEST 9 LOW LEVEL

Let ("vpii1",
   Fun ("vpii2", "vni3",
    Let ("vqi4", Int 1,
     Let ("vri5", Int 180,
      Let ("vti6", Int 60,
       Let ("vji7", Int 2,
        Let ("vloopi8",
         Fun ("vloopi9", "vni10",
          Fun ("vi11", "vqi12",
           Fun ("vi13", "vri14",
            Fun ("vi15", "vti16",
             Fun ("vi17", "vji18",
              Ifte (BOpr (Gt, Var "vni10", Int 0),
               Let ("vui19",
                BOpr (Mul,
                 BOpr (Mul, Int 3,
                  BOpr (Add, BOpr (Mul, Int 3, Var "vji18"), Int 1)),
                 BOpr (Add, BOpr (Mul, Int 3, Var "vji18"), Int 2)),
                Let ("vyi20",
                 BOpr (Div,
                  BOpr (Add,
                   BOpr (Mul, Var "vqi12",
                    BOpr (Sub, BOpr (Mul, Int 27, Var "vji18"), Int 12)),
                   BOpr (Mul, Int 5, Var "vri14")),
                  BOpr (Mul, Int 5, Var "vti16")),
                 Seq (Trace (Var "vyi20"),
                  Let ("vqi21",
                   BOpr (Mul,
                    BOpr (Mul, BOpr (Mul, Int 10, Var "vqi12"), Var "vji18"),
                    BOpr (Sub, BOpr (Mul, Int 2, Var "vji18"), Int 1)),
                   Let ("vri22",
                    BOpr (Mul, BOpr (Mul, Int 10, Var "vui19"),
                     BOpr (Sub,
                      BOpr (Add,
                       BOpr (Mul, Var "vqi12",
                        BOpr (Sub, BOpr (Mul, Int 5, Var "vji18"), Int 2)),
                       Var "vri14"),
                      BOpr (Mul, Var "vyi20", Var "vti16"))),
                    Let ("vti23", BOpr (Mul, Var "vti16", Var "vui19"),
                     Let ("vji24", BOpr (Add, Var "vji18", Int 1),
                      App
                       (App
                         (App
                           (App
                             (App (Var "vloopi9",
                               BOpr (Sub, Var "vni10", Int 1)),
                             Var "vqi21"),
                           Var "vri22"),
                         Var "vti23"),
                       Var "vji24")))))))),
               Unit)))))),
         App
          (App
            (App (App (App (Var "vloopi8", Var "vni3"), Var "vqi4"),
              Var "vri5"),
            Var "vti6"),
          Var "vji7"))))))),
   App (Var "vpii1", Int 6))

-}