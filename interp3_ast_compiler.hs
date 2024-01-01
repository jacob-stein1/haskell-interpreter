import Interp3_Ast_Translate
import Interp3_Ast_Tests
import Interp2_Parsing as L
import Interp2_Eval

decompileCom :: Com -> String
decompileCom com =
  case com of
    L.Push const -> "Push " ++ decompileConst const ++ "; "
    L.Pop -> "Pop; "
    L.Trace -> "Trace; "
    L.Add -> "Add; "
    L.Sub -> "Sub; "
    L.Mul -> "Mul; "
    L.Div -> "Div; "
    L.And -> "And; "
    L.Or -> "Or; "
    L.Not -> "Not; "
    L.Lt -> "Lt; "
    L.Gt -> "Gt; "
    L.Swap -> "Swap; "
    L.Call -> "Call; "
    L.Return -> "Return; "
    L.Bind -> "Bind; "
    L.Lookup -> "Lookup; "
    L.Ifte ifCom elseCom -> "If " ++ decompile ifCom ++ "Else " ++ decompile elseCom ++ "End; "
    L.Fun funCom -> "Fun " ++ decompile funCom ++ "Swap; Return; End; "

decompileConst :: Const -> String
decompileConst const =
  case const of
    IntConst n -> show n
    BoolConst b -> show b
    UnitConst -> "Unit"
    Sym s -> s

decompile :: Prog -> String
decompile = concatMap decompileCom

evalWrap :: String -> IO Trace
evalWrap s = do
    result <- parseProgram s
    case result of
        Left parseError -> return []
        Right prog -> return (eval [] [] [] prog)

main :: IO ()
main = do
  putStrLn "Test 1:"
  evalWrap (decompile (translateAst test1)) >>= print

  putStrLn "\nTest 2:"
  evalWrap (decompile (translateAst test2)) >>= print

  putStrLn "\nTest 3:"
  evalWrap (decompile (translateAst test3)) >>= print

  putStrLn "\nTest 4:"
  evalWrap (decompile (translateAst test4)) >>= print

  putStrLn "\nTest 5:"
  evalWrap (decompile (translateAst test5)) >>= print

  putStrLn "\nTest 6:"
  evalWrap (decompile (translateAst test6)) >>= print

  putStrLn "\nTest 7:"
  evalWrap (decompile (translateAst test7)) >>= print

  putStrLn "\nTest 8:"
  evalWrap (decompile (translateAst test8)) >>= print

  putStrLn "\nTest 9:"
  evalWrap (decompile (translateAst test9)) >>= print