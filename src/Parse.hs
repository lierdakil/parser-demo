module Parse (
    parser
  , Terminal(..)
  , NonTerminal(..)
  , Symbol(..)
  , Rule
  , Rules
  , Precedence
  , Assoc(..)
  , Associativity
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec as P
import Control.Monad

data Terminal = Terminal String | Epsilon | Eof deriving (Eq, Ord, Show)
data NonTerminal = StartRule | NonTerminal String deriving (Eq, Ord, Show)
data Symbol = STerminal Terminal | SNonTerminal NonTerminal deriving (Eq, Ord, Show)
type Rule = (NonTerminal, [[Symbol]])
type Rules = M.Map NonTerminal [[Symbol]]
type Precedence = [S.Set Terminal]
data Assoc = LeftAssoc | RightAssoc deriving (Eq, Show)
type Associativity = M.Map Terminal Assoc


parser :: String -> Either ParseError (Rules, Precedence, Associativity)
parser = parse ((,,) <$> (rules <* spaces) <*> (precedence <* spaces) <*> (associativity <* spaces) <* eof ) "input"

rules :: Parser Rules
rules = M.fromListWith (++) . addStart <$> manyTill (rule <* optional newline) (void newline <|> eof)

rule :: Parser Rule
rule = (,) <$> (nonTerminal <* (string "=" <|> string "->") <* spaces') <*> (many symbol `sepBy` (char '|' <* spaces'))

nonTerminal :: Parser NonTerminal
nonTerminal = (NonTerminal <$> ((:) <$> upper <*> many someChar)) <* spaces'

terminal :: Parser Terminal
terminal = (
      Terminal <$> ((:) <$> lower <*> many someChar)
  <|> Terminal <$> (char '"' *> many (noneOf "\"") <* char '"')
  ) <* spaces'

symbol :: Parser Symbol
symbol = (SNonTerminal <$> nonTerminal) <|> (STerminal <$> terminal)

spaces' :: Parser ()
spaces' = skipMany $ char ' '

someChar :: Parser Char
someChar = noneOf " \n"

addStart :: [Rule] -> [Rule]
addStart xs@((h, _):_) = (StartRule, [[SNonTerminal h, STerminal Eof]]):xs
addStart xs = xs

precedence :: Parser Precedence
precedence = manyTill (S.fromList <$> terminal `sepBy1` spaces' <* optional newline) (void newline <|> eof)

associativity :: Parser Associativity
associativity = M.fromList . concat <$> manyTill (( map . flip (,) <$> assoc <*> many1 terminal) <* optional newline) (void newline <|> eof)

assoc :: Parser Assoc
assoc = ((RightAssoc <$ string "Right") <|> (LeftAssoc <$ string "Left")) <* spaces'
