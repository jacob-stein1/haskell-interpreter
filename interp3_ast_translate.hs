module Interp3_Ast_Translate where

import Interp3_Ast_Tests as H
import Interp2_Parsing as L

translateExpr :: Expr -> Prog
translateExpr (Int n) = [Push (IntConst n)]
translateExpr (Bool b) = [Push (BoolConst b)]
translateExpr Unit = [Push UnitConst]
translateExpr (UOpr (opr, m)) = translateUnaryOperator opr m
translateExpr (BOpr (opr, m, n)) = translateBinaryOperator opr m n
translateExpr (Var s) = translateVariable s
translateExpr (H.Fun (f, x, m)) = translateFunction f x m
translateExpr (App (m, n)) = translateApplication m n
translateExpr (Let (x, m, n)) = translateLet x m n
translateExpr (Seq (m, n)) = translateSequence m n
translateExpr (H.Ifte (m, n1, n2)) = translateIfElse m n1 n2
translateExpr (H.Trace m) = translateTrace m

translateUnaryOperator :: UOpr -> Expr -> Prog
translateUnaryOperator Neg m = translateExpr m ++ [Push (IntConst (-1)), L.Mul]
translateUnaryOperator H.Not m = translateExpr m ++ [L.Not]

translateBinaryOperator :: BOpr -> Expr -> Expr -> Prog
translateBinaryOperator H.Add m n = translateExpr m ++ translateExpr n ++ [L.Add]
translateBinaryOperator H.Sub m n = translateExpr m ++ translateExpr n ++ [Swap, L.Sub]
translateBinaryOperator H.Mul m n = translateExpr m ++ translateExpr n ++ [L.Mul]
translateBinaryOperator H.Div m n = translateExpr m ++ translateExpr n ++ [Swap, L.Div]
translateBinaryOperator Mod m n =
  translateExpr n ++ translateExpr m ++ [L.Div] ++ translateExpr n ++ [L.Mul] ++ translateExpr m ++ [L.Sub]
translateBinaryOperator H.And m n = translateExpr m ++ translateExpr n ++ [L.And]
translateBinaryOperator H.Or m n = translateExpr m ++ translateExpr n ++ [L.Or]
translateBinaryOperator H.Lt m n = translateExpr m ++ translateExpr n ++ [Swap, L.Lt]
translateBinaryOperator H.Gt m n = translateExpr m ++ translateExpr n ++ [Swap, L.Gt]
translateBinaryOperator Lte m n = translateExpr m ++ translateExpr n ++ [Swap, L.Gt, L.Not]
translateBinaryOperator Gte m n = translateExpr m ++ translateExpr n ++ [Swap, L.Lt, L.Not]
translateBinaryOperator Eq m n =
  translateExpr m ++ translateExpr n ++ [Swap, L.Gt, L.Not] ++ translateExpr m ++ translateExpr n ++ [Swap, L.Lt, L.Not, L.And]

translateVariable :: String -> Prog
translateVariable s = [Push (Sym s), Lookup]

translateFunction :: String -> String -> Expr -> Prog
translateFunction f x m = [Push (Sym f), L.Fun ([Push (Sym x), Bind] ++ translateExpr m)]

translateApplication :: Expr -> Expr -> Prog
translateApplication m n = translateExpr m ++ translateExpr n ++ [Swap, Call]

translateLet :: String -> Expr -> Expr -> Prog
translateLet x m n = translateExpr m ++ [Push (Sym x), Bind] ++ translateExpr n

translateSequence :: Expr -> Expr -> Prog
translateSequence m n = translateExpr m ++ [Pop] ++ translateExpr n

translateIfElse :: Expr -> Expr -> Expr -> Prog
translateIfElse m n1 n2 = translateExpr m ++ [L.Ifte (translateExpr n1) (translateExpr n2)]

translateTrace :: Expr -> Prog
translateTrace m = translateExpr m ++ [L.Trace]

translateAst :: Expr -> Prog
translateAst expr = translateExpr expr