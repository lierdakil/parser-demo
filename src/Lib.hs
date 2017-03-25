{-# LANGUAGE MultiWayIf, FlexibleContexts #-}
module Lib (module Lib, module Parse) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Array
import Control.Monad.State

import Data.List
import Parse

type LL1Table = Array (Int, Int) [(NonTerminal, [Symbol])]

{- first ε = {ε}
   first Tα | T is terminal = {T}
            | T is nonterminal and ε ∈ ∪ ∀ T -> β. first β = ∪ ∀ T -> β. first β ∪ first α
            | T is nonterminal and ε ∉ ∪ ∀ T -> β. first β = ∪ ∀ T -> β. first β
-}
first :: Rules -> [Symbol] -> S.Set Terminal
first _rules [] = S.singleton Epsilon
first _rules (STerminal x:_) = S.singleton x
first rules (SNonTerminal x:xs)
  | Epsilon `S.member` fx = S.delete Epsilon fx `S.union` first rules xs
  | otherwise = fx
  where
    fx = S.unions $ map (first rules) $ fromMaybe [] pr
    pr = M.lookup x rules

{- "S" is a start rule by convention.
    follow S = EOF
    follow T = ∪ for each production rule A -> αTβ
        | ε ∈ first β and T ≠ A = first β \ {ε} ∪ follow A
        | otherwise = first β \ {ε}
-}
follow :: Rules -> NonTerminal -> S.Set Terminal
follow _rules (NonTerminal "S") = S.singleton Eof
follow rules nt = S.unions $ concatMap (uncurry $ map . fol) els
  where
    els = M.toList rules
    fol _p [] = S.empty
    fol p (SNonTerminal nt':xs)
      | nt == nt' =
           if S.member Epsilon fxs && p /= nt'
           then S.delete Epsilon fxs `S.union` follow rules p
           else S.delete Epsilon fxs
      where
        fxs = first rules xs
    fol p (_:xs) = fol p xs

{-  table LL[A, a] where A is production rule head and a is a terminal (not ε),
    if ∀ A -> α, a ∈ first α, then A -> α ∈ LL[A, a]
    if ∀ A -> α, ε ∈ first α, then ∀ b ∈ follow A, A -> α ∈ LL[A, b]
-}
makeLL1 :: Rules -> LL1Table
makeLL1 rules = accumArray (++) [] ((0, 0), (S.size nterms - 1, S.size terms - 1)) assoc
  where
    terms = allTerminals rules
    nterms = allNonTerminals rules
    assoc = concatMap (uncurry (concatMap . assocRule)) (M.toList rules)
    assocRule p alpha = concatMap termCell $ S.toList fa
      where
        fa = first rules alpha
        fol = follow rules p
        termCell Epsilon = concatMap termCell $ S.toList fol
        termCell term = [((S.findIndex p nterms, S.findIndex term terms), [(p, alpha)])]

stepLL1FA :: MonadState ([Symbol], [Terminal]) m => Rules -> Array (Int, Int) [(a, [Symbol])] -> m (Either String (Either Terminal ((Int, Int), (a, [Symbol]))))
stepLL1FA rules tbl = do
  a <- peek
  x <- pop
  let ti = S.findIndex a at
  case x of
    STerminal a' | a == a' -> do
      shift
      return $ Right $ Left a
    STerminal Epsilon ->
      return $ Right $ Left Epsilon
    STerminal t -> return $ Left (showTerm t)
    SNonTerminal nt -> do
      let nti = S.findIndex nt ant
      case tbl ! (nti, ti) of
        [] -> return $ Left "empty action"
        [(p, alpha)] -> do
          push alpha
          return $ Right $ Right ((nti,ti), (p, alpha))
        _ -> return $ Left "ambigous"
  where
    at = allTerminals rules
    ant = allNonTerminals rules

showTerm :: Terminal -> String
showTerm (Terminal s) = s
showTerm Epsilon = "ε"
showTerm Eof = "$"

showNT :: NonTerminal -> String
showNT (NonTerminal s) = s

showSym :: Symbol -> String
showSym (STerminal t) = showTerm t
showSym (SNonTerminal t) = showNT t

showRule :: (NonTerminal, [Symbol]) -> String
showRule (nt, alts) = showNT nt ++ " → " ++ unwords (map showSym alts)

showRules :: (NonTerminal, [[Symbol]]) -> String
showRules (nt, alts) = showNT nt ++ " → " ++ intercalate "|" (map (unwords . map showSym) alts)

pop :: MonadState ([a], t) m => m a
pop = do
  (stack, input) <- get
  let (h:t) = stack
  put (t, input)
  return h

push :: MonadState ([a], t) m => [a] -> m ()
push xs = do
  (stack, input) <- get
  put (xs ++ stack, input)

peek :: MonadState (a, [t]) m => m t
peek = do
  (_, input) <- get
  return $ head input

shift :: MonadState (a, [t]) m => m ()
shift = do
  (stack, input) <- get
  put (stack, tail input)

allTerminals :: Rules -> S.Set Terminal
allTerminals r = S.fromList $ concatMap terms' $ M.elems r
  where
    terms' = concatMap terms
    terms [] = []
    terms (STerminal Epsilon:xs) = terms xs
    terms (STerminal x:xs) = x : terms xs
    terms (_:xs) = terms xs

allNonTerminals :: Rules -> S.Set NonTerminal
allNonTerminals r = S.fromList $ M.keys r ++ concatMap nterms' (M.elems r)
  where
    nterms' = concatMap nterms
    nterms [] = []
    nterms (SNonTerminal x:xs) = x : nterms xs
    nterms (_:xs) = nterms xs
