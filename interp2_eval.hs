module Interp2_Eval ( eval ) where

import Interp2_Parsing
import System.IO
import Control.Monad

search :: String -> Env -> Maybe Const
search _ [] = Nothing
search sym ((x, value) : env)
  | sym == x = Just value
  | otherwise = lookup sym env

eval :: Stack -> Env -> Trace -> Prog -> Trace
eval _ _ t [] = t  -- termination returns trace
eval s e t (Push c : p) = eval (c : s) e t p
eval (_ : s0) e t (Pop : p) = eval s0 e t p  -- PopStack
eval [] e t (Pop : p) = eval [] e ("Panic1" : t) []  -- PopError
eval (c : s0) e t (Trace : p) = eval (UnitConst : s0) e (show c : t) p  -- TraceStack
eval [] e t (Trace : p) = eval [] e ("Panic2" : t) []  -- TraceError

eval (IntConst i : IntConst j : s0) e t (Add : p) = eval (IntConst (i + j) : s0) e t p  -- AddStack
eval (_ : _ : s0) e t (Add : p) = eval [] e ("Panic3" : t) []  -- AddError1
eval [] e t (Add : p) = eval [] e ("Panic4" : t) []  -- AddError2
eval [_] e t (Add : p) = eval [] e ("Panic5" : t) []  -- AddError3

eval (IntConst i : IntConst j : s0) e t (Sub : p) = eval (IntConst (i - j) : s0) e t p  -- SubStack
eval (_ : _ : s0) e t (Sub : p) = eval [] e ("Panic6" : t) []  -- SubError1
eval [] e t (Sub : p) = eval [] e ("Panic7" : t) []  -- SubError2
eval [_] e t (Sub : p) = eval [] e ("Panic8" : t) []  -- SubError3

eval (IntConst i : IntConst j : s0) e t (Mul : p) = eval (IntConst (i * j) : s0) e t p  -- MulStack
eval (_ : _ : s0) e t (Mul : p) = eval [] e ("Panic9" : t) []  -- MulError1
eval [] e t (Mul : p) = eval [] e ("Panic10" : t) []  -- MulError2
eval [_] e t (Mul : p) = eval [] e ("Panic11" : t) []  -- MulError3

eval (IntConst i : IntConst j : s0) e t (Div : p)
  | j /= 0 = eval (IntConst (i `div` j) : s0) e t p  -- DivStack
  | otherwise = eval [] e ("Panic12" : t) []  -- DivError0
eval (_ : _ : s0) e t (Div : p) = eval [] e ("Panic13" : t) []  -- DivError1
eval [] e t (Div : p) = eval [] e ("Panic14" : t) []  -- DivError2
eval [_] e t (Div : p) = eval [] e ("Panic15" : t) []  -- DivError3

eval (BoolConst a : BoolConst b : s0) e t (And : p) = eval (BoolConst (a && b) : s0) e t p  -- AndStack
eval (_ : _ : s0) e t (And : p) = eval [] e ("Panic16" : t) []  -- AndError1
eval [] e t (And : p) = eval [] e ("Panic17" : t) []  -- AndError2
eval [_] e t (And : p) = eval [] e ("Panic18" : t) []  -- AndError3

eval (BoolConst a : BoolConst b : s0) e t (Or : p) = eval (BoolConst (a || b) : s0) e t p  -- OrStack
eval (_ : _ : s0) e t (Or : p) = eval [] e ("Panic19" : t) []  -- OrError1
eval [] e t (Or : p) = eval [] e ("Panic20" : t) []  -- OrError2
eval [_] e t (Or : p) = eval [] e ("Panic21" : t) []  -- OrError3

eval (BoolConst a : s0) e t (Not : p) = eval (BoolConst (not a) : s0) e t p  -- NotStack
eval (_ : s0) e t (Not : p) = eval [] e ("Panic22" : t) []  -- NotError1
eval [] e t (Not : p) = eval [] e ("Panic23" : t) []  -- NotError2

eval (IntConst i : IntConst j : s0) e t (Lt : p) = eval (BoolConst (i < j) : s0) e t p  -- LtStack
eval (_ : _ : s0) e t (Lt : p) = eval [] e ("Panic24" : t) []  -- LtError1
eval [] e t (Lt : p) = eval [] e ("Panic25" : t) []  -- LtError2
eval [_] e t (Lt : p) = eval [] e ("Panic26" : t) []  -- LtError3

eval (IntConst i : IntConst j : s0) e t (Gt : p) = eval (BoolConst (i > j) : s0) e t p  -- GtStack
eval (_ : _ : s0) e t (Gt : p) = eval [] e ("Panic27" : t) []  -- GtError1
eval [] e t (Gt : p) = eval [] e ("Panic28" : t) []  -- GtError2
eval [_] e t (Gt : p) = eval [] e ("Panic29" : t) []  -- GtError3

eval (c1 : c2 : s0) e t (Swap : p) = eval (c2 : c1 : s0) e t p  -- SwapStack
eval [_] e t (Swap : p) = eval [] e ("Panic30" : t) []  -- SwapError1
eval [] e t (Swap : p) = eval [] e ("Panic31" : t) []  -- SwapError2

eval (BoolConst True : s0) e t ((Ifte coms1 _) : p) = eval s0 e t (coms1 ++ p)  -- ThenStack
eval (BoolConst False : s0) e t ((Ifte _ coms2) : p) = eval s0 e t (coms2 ++ p)  -- ElseStack
eval (_ : s0) e t ((Ifte _ _) : p) = eval [] e ("Panic32" : t) []  -- IfElseError1
eval [] e t ((Ifte _ _): p) = eval [] e ("Panic33" : t) []  -- IfElseError2

eval (Sym x : v : s0) e t (Bind : p) = eval s0 ((x, v) : e) t p  -- BindStack
eval (_ : v : s0) e t (Bind : p) = eval [] e ("Panic34" : t) []  -- BindError1
eval [_] e t (Bind : p) = eval [] e ("Panic35" : t) []  -- BindError3
eval [] e t (Bind : p) = eval [] e ("Panic36" : t) []  -- BindError2

eval (Sym x : s0) e t (Lookup : p) =
  case search x e of
    Just v -> eval (v : s0) e t p  -- LookupStack
    Nothing -> eval [] e ("Panic37" : t) []  -- LookupError4
eval (_ : s0) e t (Lookup : p) = eval [] e ("Panic38" : t) []  -- LookupError1
eval [] e t (Lookup : p) = eval [] e ("Panic39" : t) []  -- LookupError3

eval (Sym x : s0) e t (Fun coms : p) =
  eval (Closure (x, e, coms) : s0) e t p  -- FunStack
eval (_ : s0) _ t (Fun _ : _) = eval [] [] ("Panic40" : t) []  -- FunError1
eval [] _ t (Fun _ : _) = eval [] [] ("Panic41" : t) []  -- FunError2

eval (Closure (f, ef, coms) : a : s0) e t (Call : p0) =
  eval (a : Closure ("cc", e, p0) : s0) ((f, Closure (f, ef, coms)) : ef) t coms  -- CallStack
eval (_ : []) _ t (Call : _) = eval [] [] ("Panic42" : t) []  -- CallError3
eval (_ : s0) _ t (Call : _) = eval [] [] ("Panic43" : t) []  -- CallError1
eval [] _ t (Call : _) = eval [] [] ("Panic44" : t) []  -- CallError2

eval (Closure (f, ef, coms) : a : s0) e t (Return : p0) =
  eval (a : s0) ef t coms  -- ReturnStack
eval (_ : []) _ t (Return : _) = eval [] [] ("Panic45" : t) []  -- ReturnError3
eval (_ : s0) _ t (Return : _) = eval [] [] ("Panic46" : t) []  -- ReturnError1
eval [] _ t (Return : _) = eval [] [] ("Panic47" : t) []  -- ReturnError2

testInterpreter :: String -> IO ()
testInterpreter input = do
  putStrLn $ "Input: " ++ input
  result <- parseProgram input
  case result of
    Left parseError -> do
      putStrLn $ "Parse Error: " ++ show parseError
      putStrLn "-------------------------------------"
    Right prog -> do
      let trace = eval [] [] [] (prog)
      putStrLn $ "Trace: " ++ show trace
      putStrLn "-------------------------------------"

readTests :: IO ()
readTests = do
  forM_ [1..10] $ \fileNumber -> do
    let filePath = "./tests/interp2test" ++ show fileNumber ++ ".txt"
    putStrLn $ "Processing file: " ++ filePath
    contents <- readFile filePath
    testInterpreter contents

main :: IO ()
main = readTests