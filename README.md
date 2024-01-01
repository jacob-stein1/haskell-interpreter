# Haskell Interpreter

## Module: `Interp1_Parsing.hs`

### Types

#### `Const`

- Represents constants in the low-level language.
- Possible values:
  - `IntConst Int`: Integer constant.
  - `BoolConst Bool`: Boolean constant (`True` or `False`).
  - `UnitConst`: Unit constant.

#### `Com`

- Represents commands in the low-level language.
- Possible values:
  - `Push Const`: Pushes a constant onto the stack.
  - `Pop`: Pops the top element from the stack.
  - `Trace`: Duplicates the top of the stack and adds it to the trace.
  - `Add`, `Sub`, `Mul`, `Div`: Arithmetic operations.
  - `And`, `Or`, `Not`: Logical operations.
  - `Lt`, `Gt`: Comparison operations.

### Parsers

#### `whitespace`

- Parser for skipping white spaces.

#### `constParser`

- Parser for constants.
- Parses integer constants, Boolean constants (`True` or `False`), and unit constant (`Unit`).

#### `comParser`

- Parser for commands.
- Parses commands such as `Push`, `Pop`, arithmetic operations, logical operations, and comparison operations.

#### `programParser`

- Parser for an entire program.
- Parses a sequence of commands separated by semicolons.

### Functions

#### `parseProgram`

- Takes a string input representing a low-level program.
- Returns either a list of commands or a parsing error using `Either ParseError [Com]`.

## File: `Interp1_Parsing.hs`

### Stack Manipulation Commands

- `Push`: Pushes a constant onto the stack.
- `Pop`: Pops the top element from the stack.
- `Trace`: Duplicates the top of the stack and adds it to the trace.

### Arithmetic Commands

- `Add`: Adds the top two integers on the stack.
- `Sub`: Subtracts the second integer on the stack from the top integer.
- `Mul`: Multiplies the top two integers on the stack.
- `Div`: Divides the top integer on the stack by the second integer.

### Logical Commands

- `And`: Performs a logical AND operation on the top two Boolean values on the stack.
- `Or`: Performs a logical OR operation on the top two Boolean values on the stack.
- `Not`: Negates the top Boolean value on the stack.

### Comparison Commands

- `Lt`: Checks if the second integer on the stack is less than the top integer.
- `Gt`: Checks if the second integer on the stack is greater than the top integer.

## Module: `Interp2_Parsing.hs`

### Types

#### `New Const Values`

- Represents constants in the Interp2 language.
- Adds new values:
  - `Sym String`: Symbol constant.
  - `Closure (String, Env, [Com])`: Closure constant.

#### `Com`

- Represents commands in the Interp2 language.
- Adds new commands:
  - `Swap`: Swaps the top two elements on the stack.
  - `Call`: Calls a closure on the top of the stack.
  - `Return`: Returns from a closure.
  - `Bind`: Binds a symbol to a value in the environment.
  - `Lookup`: Looks up a symbol in the environment.
  - `Ifte [Com] [Com]`: Conditional execution.
  - `Fun [Com]`: Defines a function.

#### `Env`

- Represents the environment in the low-level language.
- Associates symbols with constants.

### Parsers

#### `ifteParser`

- Parser for the `Ifte` command.
- Parses conditional branches.

#### `funParser`

- Parser for the `Fun` command.
- Parses function definitions.

## Module: `Interp2_Eval.hs`

### Functions

#### `eval`

- Takes a stack, environment, trace, and program as input.
- Evaluates the program and returns a trace.
- Handles all previous commands with the addition of stack manipulations (`Swap`), conditional execution (`Ifte`), function definitions (`Fun`), and function calls (`Call` and `Return`).

#### `search`

- Helper function to search for a symbol in the environment.

### Example Usage

```haskell
import Interp2_Eval

main :: IO ()
main = do
  let inputProgram = "Push 1; Bind x; Fun Push x; Call; Return; Trace;"
  result <- parseProgram inputProgram
  case result of
    Left parseError -> putStrLn $ "Parse Error: " ++ show parseError
    Right commands -> do
      let trace = eval [] [] [] commands
      putStrLn $ "Trace: " ++ show trace
```

## Module: `Interp3_Ast_Tests.hs`

### Overview

- Contains the AST data types for an OCaml-like functional programming language
- Parser for high-level language is not included
- ASTs for test cases were obtained from CS 320 AST representation in OCaml and reconstructed in Haskell

### Data Types

#### `UOpr` and `BOpr`

- Enumerations representing unary and binary operators, respectively.

#### `Expr`

- Represents the abstract syntax tree (AST) for a high-level language.
- Includes expressions for integers, booleans, unit, unary and binary operators, variables, function definitions, function applications, let bindings, sequence of expressions, conditional (`if-then-else`), and tracing.

## Module: `Interp3_Ast_Translate.hs`

### Functions

#### `translateExpr`

- Translates an expression in the high-level AST into a low-level AST from interp2.
- Handles different AST node types by calling specialized translation functions.

#### `translateUnaryOperator`

- Translates unary operators (Neg and Not) into corresponding low-level instructions.

#### `translateBinaryOperator`

- Translates binary operators (Add, Sub, Mul, Div, Mod, And, Or, Lt, Gt, Lte, Gte, Eq) into corresponding low-level instructions.

#### `translateVariable`

- Translates a variable into a sequence of instructions to perform a lookup in the environment.

#### `translateFunction`

- Translates a function definition into a sequence of instructions to create a closure.

#### `translateApplication`

- Translates a function application into a sequence of instructions to execute the function.

#### `translateLet`

- Translates a let binding into a sequence of instructions to bind a value to a variable.

#### `translateSequence`

- Translates a sequence of expressions into a sequence of instructions, popping the result of the first expression before executing the second.

#### `translateIfElse`

- Translates an if-then-else expression into a sequence of instructions.

#### `translateTrace`

- Translates a trace expression into a sequence of instructions.

#### `translateAst`

- Translates an entire Interp3 AST into a sequence of instructions.

## Module: `Interp3_Ast_Compiler.hs`

### Functions

#### `decompileCom`

- Decompiles a single command (Com) in the low-level AST into a human-readable string.
- Handles different command types and their corresponding decompilation.

#### `decompileConst`

- Decompiles a constant (Const) in the low-level AST into a human-readable string.
- Handles different constant types.

#### `decompile`

- Decompiles an entire low-level AST program into a human-readable string.
- Utilizes `decompileCom` to decompile individual commands.

#### `evalWrap`

- Parses a string representation of an decompiled low-level program program, translates it into an low-level AST, and evaluates it using the low-level interpreter.
- Returns the trace of the evaluation.
- Execute high level programs using: evalWrap (decompile (translateAST (highLevelParser high_level_program)))
- Parsed high-level AST is translated into low-level AST, which is decompiled into a string program, which is evaluated by evalWrap
- All functions are provided except highLevelParser
