{-# LANGUAGE MultiWayIf, FlexibleContexts #-}
module Lib where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Array
import Data.List
import Data.Tree
import Control.Monad.State
-- import Control.Monad.Trans.State
-- import Text.Regex.TDFA
-- import Text.Show.Pretty

data Terminal = Terminal String | Epsilon | Eof deriving (Eq, Ord, Show)
newtype NonTerminal = NonTerminal String deriving (Eq, Ord, Show)
data Symbol = STerminal Terminal | SNonTerminal NonTerminal deriving (Eq, Ord, Show)
type Rules = M.Map NonTerminal [[Symbol]]
type LL1Table = Array (Int, Int) [(NonTerminal, [Symbol])]

first :: Rules -> [Symbol] -> S.Set Terminal
first _rules [] = S.singleton Epsilon
first _rules (STerminal x:_) = S.singleton x
first rules (SNonTerminal x:xs) =
  let fx = S.unions $ map (first rules) $ fromMaybe [] pr
      pr = M.lookup x rules
  in if Epsilon `S.member` fx
     then S.delete Epsilon fx `S.union` first rules xs
     else fx

follow :: Rules -> NonTerminal -> S.Set Terminal
follow rules nt
  | (NonTerminal "S") <- nt = S.singleton Eof
  | otherwise = S.unions $ concatMap (uncurry $ fol' nt) els
  where
    els = M.toList rules
    fol' nt' p = map (fol nt' p)
    fol :: NonTerminal -> NonTerminal -> [Symbol] -> S.Set Terminal
    fol _search _p [] = S.empty
    fol search p (SNonTerminal nt':xs)
      | search == nt' =
        let fxs = first rules xs
        in if S.member Epsilon fxs && p /= nt'
           then S.delete Epsilon fxs `S.union` follow rules p
           else S.delete Epsilon fxs
    fol search p (_:xs) = fol search p xs

allTerminals :: Rules -> S.Set Terminal
allTerminals r = S.fromList $ concatMap terms' $ M.elems r
  where
    terms' = concatMap terms
    terms [] = []
    terms (STerminal x:xs) = x : terms xs
    terms (_:xs) = terms xs

allNonTerminals :: Rules -> S.Set NonTerminal
allNonTerminals r = S.fromList $ M.keys r ++ concatMap nterms' (M.elems r)
  where
    nterms' = concatMap nterms
    nterms [] = []
    nterms (SNonTerminal x:xs) = x : nterms xs
    nterms (_:xs) = nterms xs

makeLL1 :: Rules -> LL1Table
makeLL1 rules = accumArray (++) [] ((0, 0), (S.size nterms - 1, S.size terms - 1)) assoc
  where
    terms = allTerminals rules
    nterms = allNonTerminals rules
    assoc = concatMap (uncurry assocRule') (M.toList rules)
    assocRule' p = concatMap (assocRule p)
    assocRule p alpha = termCells
      where
        fa = first rules alpha
        fol = follow rules p
        termCells = concatMap termCell $ S.toList fa
        termCell Epsilon = concatMap termCell $ S.toList fol
        termCell term = [((S.findIndex p nterms, S.findIndex term terms), [(p, alpha)])]

test1 :: Rules
test1 = M.fromList [
    (nt' "S", [[nt "E", eof]])
  , (nt' "E", [[nt "T", nt "E'"]])
  , (nt' "E'", [[t "+", nt "T", nt "E'"], [eps]])
  , (nt' "T", [[nt "F", nt "T'"]])
  , (nt' "T'", [[t "*", nt "F", nt "T'"], [eps]])
  , (nt' "F", [[t "(", nt "E", t ")"], [t "id"]])
  ]
  where
    nt' = NonTerminal
    nt = SNonTerminal . NonTerminal
    t = STerminal . Terminal
    eps = STerminal Epsilon
    eof = STerminal Eof

runTest1 :: IO ()
runTest1 = printLL1 test1 $ makeLL1 test1

printLL1 :: Rules -> LL1Table -> IO ()
printLL1 rules tbl = do
  let
    terms = allTerminals rules
    nterms = allNonTerminals rules
    tab = "|"
    showItem (nti, ti) = intercalate ";" $ map showRule (tbl ! (nti, ti))
    ((minnti, minti), (maxnti, maxti)) = bounds tbl
    printRow nti = do
      putStr (showNT $ S.elemAt nti nterms)
      mapM_ (putStr . (tab++) . showItem . (,) nti) [minti..maxti]
      putStrLn "|"
  putStr "|"
  mapM_ (putStr . (tab++) . showTerm) $ S.toList terms
  putStrLn "|"
  putStr "|-"
  mapM_ (const $ putStr (tab++"-")) $ S.toList terms
  putStrLn "|"
  mapM_ printRow [minnti..maxnti]

type SynTree = Tree String

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

stepLL1FA :: MonadState ([Symbol], [Terminal]) m => Rules -> Array (Int, Int) [(a, [Symbol])] -> m (Either String (Either Terminal ((Int, Int), (a, [Symbol]))))
stepLL1FA rules tbl = do
  (stack, input) <- get
  let (x:xs) = stack
      (a:as) = input
      at = allTerminals rules
      ant = allNonTerminals rules
      ti = S.findIndex a at
  case x of
    STerminal a' | a == a' -> do
      put (xs, as)
      return $ Right $ Left a
    STerminal Epsilon -> do
      put (xs, a:as)
      return $ Right $ Left Epsilon
    STerminal t -> return $ Left (showTerm t)
    SNonTerminal nt -> do
      let nti = S.findIndex nt ant
      case tbl ! (nti, ti) of
        [] -> return $ Left "empty action"
        [act] -> do
          put (snd act ++ xs, a:as)
          return $ Right $ Right ((nti,ti), act)
        _ -> return $ Left "ambigous"
