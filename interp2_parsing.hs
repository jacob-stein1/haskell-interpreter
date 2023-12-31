module Interp2_Parsing
  ( Const(..)
  , Com(..)
  , Stack
  , Trace
  , Prog
  , Env
  , parseProgram
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<*), (*>))

data Const =
    IntConst Int
  | BoolConst Bool
  | UnitConst
  | Sym String
  | Closure (String, Env, [Com])
  deriving Show

type Stack = [Const]
type Trace = [String]
type Prog = [Com]
type Env = [(String, Const)]

data Com =
    Push Const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | Swap
  | Call
  | Return
  | Bind
  | Lookup
  | Ifte [Com] [Com]
  | Fun [Com]
  deriving Show

type StackParser a = ParsecT String () IO a

whitespace :: StackParser ()
whitespace = skipMany (oneOf " \t\n\r")

constParser :: StackParser Const
constParser = choice
  [ try (IntConst <$> optionalSign)
  , BoolConst True <$ string "True"
  , BoolConst False <$ string "False"
  , UnitConst <$ string "Unit"
  , Sym <$> (many1 (noneOf " \t\n\r;"))
  ]
  where
    optionalSign = do
      sign <- option "" (string "-")
      digits <- many1 digit
      return $ read (sign ++ digits)

comParser :: StackParser Com
comParser = do
  whitespace
  command <- choice
    [ try (string "And" *> notFollowedBy alphaNum) *> pure And
    , try (string "Add" *> notFollowedBy alphaNum) *> pure Add
    , try (string "Push" *> notFollowedBy alphaNum) *> (Push <$> (whitespace *> constParser))
    , try (string "Pop" *> notFollowedBy alphaNum) *> pure Pop
    , try (string "Lookup" *> notFollowedBy alphaNum) *> pure Lookup
    , try (string "Lt" *> notFollowedBy alphaNum) *> pure Lt
    , try (string "Sub" *> notFollowedBy alphaNum) *> pure Sub
    , try (string "Swap" *> notFollowedBy alphaNum) *> pure Swap
    , Mul <$ string "Mul"
    , Div <$ string "Div"
    , Or <$ string "Or"
    , Not <$ string "Not"
    , Gt <$ string "Gt"
    , Trace <$ string "Trace"
    , Call <$ string "Call"
    , Return <$ string "Return"
    , Bind <$ string "Bind"
    , try (lookAhead (string "If" *> many (oneOf " \t\n\r") *> alphaNum)) *> ifteParser
    , try (lookAhead (string "Fun" *> many (oneOf " \t\n\r") *> alphaNum)) *> funParser
    ] <* optional (string ";" *> optional whitespace)
  return command

ifteParser :: StackParser Com
ifteParser = do
  ifBranch <- between (string "If") (string "Else") (many comParser)
  elseBranch <- manyTill comParser (try (string "End"))
  return $ Ifte ifBranch elseBranch

funParser :: StackParser Com
funParser = Fun <$> between (string "Fun") (string "End") (many comParser)

parseProgram :: String -> IO (Either ParseError [Com])
parseProgram = runParserT (many comParser) () "input"