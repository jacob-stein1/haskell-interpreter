module Interp1_Parsing
  ( Const(..)
  , Com(..)
  , parseProgram
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<*), (*>))

data Const =
    IntConst Int
  | BoolConst Bool
  | UnitConst
  deriving Show

data Com =
    Push Const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
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
  ]
  where
    optionalSign = do
      sign <- option "" (string "-")
      digits <- many1 digit
      return $ read (sign ++ digits)

comParser :: StackParser Com
comParser = choice
  [ try (string "And" *> notFollowedBy alphaNum) *> pure And
  , try (string "Add" *> notFollowedBy alphaNum) *> pure Add
  , try (string "Push" *> notFollowedBy alphaNum) *> (Push <$> (whitespace *> constParser))
  , try (string "Pop" *> notFollowedBy alphaNum) *> pure Pop
  , Sub <$ string "Sub"
  , Mul <$ string "Mul"
  , Div <$ string "Div"
  , Or <$ string "Or"
  , Not <$ string "Not"
  , Lt <$ string "Lt"
  , Gt <$ string "Gt"
  , Trace <$ string "Trace"
  ] <* optional whitespace

programParser :: StackParser [Com]
programParser = spaces *> sepEndBy comParser (string ";" *> optional whitespace) <* optional whitespace <* eof

parseProgram :: String -> IO (Either ParseError [Com])
parseProgram = runParserT programParser () "input"