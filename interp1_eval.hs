import Interp1_Parsing

type Stack = [Const]
type Trace = [String]
type Prog = [Com]

eval :: Stack -> Trace -> Prog -> Trace
eval s t [] = t  -- termination returns trace
eval s t (Push c : p) = eval (c : s) t p
eval (_ : s0) t (Pop : p) = eval s0 t p  -- PopStack
eval [] t (Pop : p) = eval [] ("Panic1" : t) []  -- PopError
eval (c : s0) t (Trace : p) = eval (UnitConst : s0) (show c : t) p  -- TraceStack
eval [] t (Trace : p) = eval [] ("Panic2" : t) []  -- TraceError

eval (IntConst i : IntConst j : s0) t (Add : p) = eval (IntConst (i + j) : s0) t p  -- AddStack
eval (_ : _ : s0) t (Add : p) = eval [] ("Panic3" : t) []  -- AddError1
eval [] t (Add : p) = eval [] ("Panic4" : t) []  -- AddError2
eval [_] t (Add : p) = eval [] ("Panic5" : t) []  -- AddError3

eval (IntConst i : IntConst j : s0) t (Sub : p) = eval (IntConst (i - j) : s0) t p  -- SubStack
eval (_ : _ : s0) t (Sub : p) = eval [] ("Panic6" : t) []  -- SubError1
eval [] t (Sub : p) = eval [] ("Panic7" : t) []  -- SubError2
eval [_] t (Sub : p) = eval [] ("Panic8" : t) []  -- SubError3

eval (IntConst i : IntConst j : s0) t (Mul : p) = eval (IntConst (i * j) : s0) t p  -- MulStack
eval (_ : _ : s0) t (Mul : p) = eval [] ("Panic9" : t) []  -- MulError1
eval [] t (Mul : p) = eval [] ("Panic10" : t) []  -- MulError2
eval [_] t (Mul : p) = eval [] ("Panic11" : t) []  -- MulError3

eval (IntConst i : IntConst j : s0) t (Div : p)
  | j /= 0 = eval (IntConst (i `div` j) : s0) t p  -- DivStack
  | otherwise = eval [] ("Panic12" : t) []  -- DivError0
eval (_ : _ : s0) t (Div : p) = eval [] ("Panic13" : t) []  -- DivError1
eval [] t (Div : p) = eval [] ("Panic14" : t) []  -- DivError2
eval [_] t (Div : p) = eval [] ("Panic15" : t) []  -- DivError3

eval (BoolConst a : BoolConst b : s0) t (And : p) = eval (BoolConst (a && b) : s0) t p  -- AndStack
eval (_ : _ : s0) t (And : p) = eval [] ("Panic16" : t) []  -- AndError1
eval [] t (And : p) = eval [] ("Panic17" : t) []  -- AndError2
eval [_] t (And : p) = eval [] ("Panic18" : t) []  -- AndError3

eval (BoolConst a : BoolConst b : s0) t (Or : p) = eval (BoolConst (a || b) : s0) t p  -- OrStack
eval (_ : _ : s0) t (Or : p) = eval [] ("Panic19" : t) []  -- OrError1
eval [] t (Or : p) = eval [] ("Panic20" : t) []  -- OrError2
eval [_] t (Or : p) = eval [] ("Panic21" : t) []  -- OrError3

eval (BoolConst a : s0) t (Not : p) = eval (BoolConst (not a) : s0) t p  -- NotStack
eval (_ : s0) t (Not : p) = eval [] ("Panic22" : t) []  -- NotError1
eval [] t (Not : p) = eval [] ("Panic23" : t) []  -- NotError2

eval (IntConst i : IntConst j : s0) t (Lt : p) = eval (BoolConst (i < j) : s0) t p  -- LtStack
eval (_ : _ : s0) t (Lt : p) = eval [] ("Panic24" : t) []  -- LtError1
eval [] t (Lt : p) = eval [] ("Panic25" : t) []  -- LtError2
eval [_] t (Lt : p) = eval [] ("Panic26" : t) []  -- LtError3

eval (IntConst i : IntConst j : s0) t (Gt : p) = eval (BoolConst (i > j) : s0) t p  -- GtStack
eval (_ : _ : s0) t (Gt : p) = eval [] ("Panic27" : t) []  -- GtError1
eval [] t (Gt : p) = eval [] ("Panic28" : t) []  -- GtError2
eval [_] t (Gt : p) = eval [] ("Panic29" : t) []  -- GtError3

testInterpreter :: String -> IO ()
testInterpreter input = do
  putStrLn $ "Input: " ++ input
  result <- parseProgram input
  case result of
    Left parseError -> do
      putStrLn $ "Parse Error: " ++ show parseError
      putStrLn "-------------------------------------"
    Right prog -> do
      let trace = eval [] [] (prog)
      putStrLn $ "Trace: " ++ show trace
      putStrLn "-------------------------------------"

main :: IO ()
main = do
  testInterpreter "Push 1; Push Unit; Push True; Push False; Push -1; Trace; Pop; Trace; Pop; Trace; Pop; Trace; Pop; Trace;"
  testInterpreter "Pop;"
  testInterpreter "Trace;"
  testInterpreter "Push 1; Push -2; Add; Trace;"
  testInterpreter "Push Unit; Push 1; Add;"
  testInterpreter "Add;"
  testInterpreter "Push 1; Add;"
  testInterpreter "Push -1; Push -2; Sub; Trace;"
  testInterpreter "Push Unit; Push 1; Sub;"
  testInterpreter "Sub;"
  testInterpreter "Push 1; Sub;"
  testInterpreter "Push -1; Push 0; Mul; Trace;"
  testInterpreter "Push Unit; Push 1; Mul;"
  testInterpreter "Mul;"
  testInterpreter "Push 1; Mul;"
  testInterpreter "Push -1; Push -2; Div; Trace;"
  testInterpreter "Push 0; Push 2; Div;"
  testInterpreter "Push Unit; Push 1; Div;"
  testInterpreter "Div;"
  testInterpreter "Push 1; Div;"
  testInterpreter "Push 2; Push 0; Div; Trace;"
  testInterpreter "Push True; Push False; And; Trace;"
  testInterpreter "Push True; Push 1; And;"
  testInterpreter "And;"
  testInterpreter "Push True; And;"
  testInterpreter "Push True; Push False; Or; Trace;"
  testInterpreter "Push True; Push 1; Or;"
  testInterpreter "Or;"
  testInterpreter "Push True; Or;"
  testInterpreter "Push True; Not; Trace;"
  testInterpreter "Push 1; Not;"
  testInterpreter "Not;"
  testInterpreter "Push -1; Push 0; Lt; Trace;"
  testInterpreter "Push Unit; Push 1; Lt;"
  testInterpreter "Lt;"
  testInterpreter "Push 1; Lt;"
  testInterpreter "Push -1; Push 0; Gt; Trace;"
  testInterpreter "Push Unit; Push 1; Gt;"
  testInterpreter "Gt;"
  testInterpreter "Push 1; Gt;"


