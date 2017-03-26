module Parse where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec as P

data Terminal = Terminal String | Epsilon | Eof deriving (Eq, Ord, Show)
data NonTerminal = NonTerminal String | StartRule deriving (Eq, Ord, Show)
data Symbol = STerminal Terminal | SNonTerminal NonTerminal deriving (Eq, Ord, Show)
type Rules = M.Map NonTerminal [[Symbol]]


parser :: String -> Either ParseError Rules
parser = parse rules "input"
  where
  rules = spaces *> (M.fromListWith (++) . addStart <$> endBy rule eol) <* (spaces >> eof)
  rule = do
    spaces'
    name <- nonTerminal
    spaces'
    (() <$ char '=') <|> (() <$ string "->")
    spaces'
    alts <- sepBy (many symbol) (char '|')
    return (name, alts)
  nonTerminal = spaces' *> (
    NonTerminal <$> ((:) <$> upper <*> many someChar)
    ) <* spaces'
  terminal = spaces' *> (
        Epsilon <$ char 'ε'
    <|> Eof <$ char '$'
    <|> Terminal <$> ((:) <$> lower <*> many someChar)
    <|> Terminal <$> (char '"' *> many (noneOf "\"") <* char '"')
    ) <* spaces'
  symbol = spaces' *> (
        (SNonTerminal <$> nonTerminal)
    <|> (STerminal <$> terminal)
    ) <* spaces'
  eol = char '\n'
  spaces' = skipMany $ char ' '
  someChar = noneOf " \n"
  addStart xs@((h, _):_) = (StartRule, [[SNonTerminal h, STerminal Eof]]):xs
  addStart xs = xs
