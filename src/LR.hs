{-# LANGUAGE MultiWayIf, FlexibleContexts, QuasiQuotes, BangPatterns #-}
module LR where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Array
import Control.Monad.State

import Data.List
import Parse
import LL
import QQ
import Debug.Trace

data LRAction = LRShift Int | LRReduce (NonTerminal, [Symbol]) | LRAccept | LRError String deriving (Show)
type LRActionTable = Array (Int, Int) [LRAction]
type LRGotoTable = Array (Int, Int) (Maybe Int)
data Point = Point { pHead :: NonTerminal, pPos :: Int, pLookahead :: S.Set Terminal, pBody :: [Symbol] } deriving (Eq, Ord, Show)

(!!!) :: [a] -> Int -> Maybe a
a !!! b | b < length a = Just $ a !! b
        | otherwise = Nothing

closure :: Rules -> S.Set Point -> S.Set Point
closure rules i =
  case S.unions $ i : map go (S.toList i) of
    x | x == i -> x
      | otherwise -> closure rules x
  where
    go (Point _ p _ b) =
      case b !!! p of
        Just (SNonTerminal nt) -> add nt
        _ -> S.empty
    add nt = S.fromList $ map (Point nt 0 S.empty) $ fromMaybe [] $ M.lookup nt rules

closure1 :: Rules -> S.Set Point -> S.Set Point
closure1 rules i =
    case S.unions $ i : map go (S.toList i) of
      x | x == i -> x
        | otherwise -> closure1 rules x
  where
    go (Point h p la b) =
      case b !!! p of
        Just (SNonTerminal nt) | h /= nt -> S.unions $ map (add nt t) $ S.toList la
        _ -> S.empty
      where
        t = drop (p + 1) b
    add nt t la = S.fromList $ map (Point nt 0 (first rules (t ++ [STerminal la]))) $ fromMaybe [] $ M.lookup nt rules

goto :: t1 -> (t1 -> S.Set Point -> t) -> S.Set Point -> Symbol -> t
goto rules cl i x = cl rules $ S.map s $ S.filter f i
  where
    f (Point _ p _ b) = b !!! p == Just x
    s (Point h p la b) = Point h (p+1) la b

states :: Rules -> (Rules -> S.Set Point -> S.Set Point) -> (Int, S.Set (S.Set Point))
states rules cl = (S.findIndex start res, res)
  where
    res = S.filter (not . S.null) $ go S.empty start
    start = cl rules $ S.fromList $ map (Point (NonTerminal "S") 0 (S.singleton Eof)) startRule
    startRule = fromJust $ M.lookup (NonTerminal "S") rules
    as = S.map STerminal (allTerminals rules) `S.union` S.map SNonTerminal (allNonTerminals rules)
    go :: S.Set (S.Set Point) -> S.Set Point -> S.Set (S.Set Point)
    go acc st = next `S.union` newAcc
      where
      newSt = S.map (goto rules cl st) as S.\\ acc
      newAcc = S.insert st $ newSt `S.union` acc
      next = S.unions $ map (go newAcc) $ S.toList newSt

makeSLR :: Rules -> (Int, LRActionTable, LRGotoTable)
makeSLR rules = (startSt, accumArray (++) [] bs $ map (ap (,) action) $ range bs, array bs2 $ map (ap (,) goto') $ range bs2)
  where
    (startSt, stl) = states rules closure
    at = allTerminals rules
    ant = allNonTerminals rules
    bs = ((0, 0), (S.size stl - 1, S.size at - 1))
    bs2 = ((0, 0), (S.size stl - 1, S.size ant - 1))
    action x = action1 x ++ action2 x
    action1 (st, term)
      | gt <- goto rules closure curst (STerminal curterm)
      , not $ S.null gt
      = [LRShift $ S.findIndex gt stl]
      | otherwise
      = []
      where
        curst = S.elemAt st stl
        curterm = S.elemAt term at
    action2 (st, term)
      | ss <- S.filter (\(Point h p _ b) -> isNothing (b !!! p) && curterm `S.member` follow rules h) curst
      , not $ S.null ss
      = let e = S.elemAt 0 ss
        in [LRReduce (pHead e, pBody e)]
      | otherwise
      = []
      where
        curst = S.elemAt st stl
        curterm = S.elemAt term at
    goto' (st, nt)
      | gt <- goto rules closure curst (SNonTerminal cursym)
      , not $ S.null gt
      = Just $ S.findIndex gt stl
      | otherwise
      = Nothing
      where
        curst = S.elemAt st stl
        cursym = S.elemAt nt ant


peekstack :: MonadState ([a], t) m => m a
peekstack = do
  (stack, _) <- get
  return $ head stack

popn :: MonadState ([a], t) m => Int -> m ()
popn n = do
  (stack, input) <- get
  put (drop n stack, input)

stepLR :: MonadState ([Int], [Terminal]) m => Rules -> LRActionTable -> LRGotoTable -> m [LRAction]
stepLR rules action gotot = do
  a <- peek
  x <- peekstack
  case action ! (x, termIx a) of
    [LRShift n] -> do
      push [n]
      shift
    [LRReduce (nt, b)] -> do
      popn $ length b
      t <- peekstack
      when (nt /= NonTerminal "S") $ do
        let Just gtt = gotot ! (t, nonTermIx nt)
        push [gtt]
    _ -> return ()
  return $ action ! (x, termIx a)
  where
    terms = allTerminals rules
    nterms = allNonTerminals rules
    nonTermIx x = S.findIndex x nterms
    termIx x = S.findIndex x terms
