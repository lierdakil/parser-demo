{-# LANGUAGE MultiWayIf, FlexibleContexts #-}
module LR where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Array
import Control.Monad.State

import Parse
import LL
import Data.List

data LRAction = LRShift Int | LRReduce (NonTerminal, [Symbol]) | LRAccept deriving (Eq, Ord, Show)
type LRActionTable = Array (Int, Int) [LRAction]
type LRGotoTable = Array (Int, Int) (Maybe Int)
data Point = Point { pHead :: NonTerminal, pPos :: Int, pLookahead :: S.Set Terminal, pBody :: [Symbol] } deriving (Eq, Ord, Show)

(!!!) :: [a] -> Int -> Maybe a
a !!! b | b < length a = Just $ a !! b
        | otherwise = Nothing

stripLA :: Point -> Point
stripLA (Point h p _ b) = Point h p S.empty b

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
    go (Point _ p la b) =
      case b !!! p of
        Just (SNonTerminal nt) -> S.unions $ map (add nt t) $ S.toList la
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
    start = cl rules $ S.fromList $ map (Point StartRule 0 (S.singleton Eof)) startRule
    startRule = fromJust $ M.lookup StartRule rules
    as = S.map STerminal (allTerminals rules) `S.union` S.map SNonTerminal (allNonTerminals rules)
    go :: S.Set (S.Set Point) -> S.Set Point -> S.Set (S.Set Point)
    go acc st = next `S.union` newAcc
      where
      newSt = S.map (goto rules cl st) as S.\\ acc
      newAcc = S.insert st $ newSt `S.union` acc
      next = S.unions $ map (go newAcc) $ S.toList newSt

-- makeLR :: (Rules -> S.Set Point -> S.Set Point) -> (S.Set Point -> Terminal -> Point -> Bool) -> Rules -> (Int, LRActionTable, LRGotoTable)
makeLR :: (Rules -> S.Set Point -> S.Set Point) -> (S.Set Point -> Terminal -> Point -> Bool) -> Rules -> Precedence -> Associativity -> (Int, LRActionTable, LRGotoTable)
makeLR cl fol rules priorities assoc = (startSt, accumArray (++) [] bs $ map (ap (,) action) $ range bs, array bs2 $ map (ap (,) goto') $ range bs2)
  where
    (startSt, stl) = states rules cl
    at = allTerminals rules
    ant = allNonTerminals rules
    bs = ((0, 0), (S.size stl - 1, S.size at - 1))
    bs2 = ((0, 0), (S.size stl - 1, S.size ant - 1))
    action x =
      case action1 x ++ action2 x of
        y@[LRShift st, LRReduce (h, b)]
          | Just (STerminal lastTerm) <- find findTerm $ reverse b
          , Just shiftPrio <- findIndex (term `S.member`) priorities
          , Just reducePrio <- findIndex (lastTerm `S.member`) priorities
          -> let shiftAssoc = fromMaybe LeftAssoc $ M.lookup term assoc
                 reduceAssoc = fromMaybe LeftAssoc $ M.lookup lastTerm assoc
             in if
                | shiftPrio < reducePrio -> [LRShift st]
                | shiftPrio > reducePrio -> [LRReduce (h, b)]
                | shiftPrio == reducePrio && shiftAssoc == reduceAssoc ->
                  case shiftAssoc of
                    LeftAssoc -> [LRReduce (h, b)]
                    RightAssoc -> [LRShift st]
                | otherwise -> y
        [LRReduce (StartRule, _)] -> [LRAccept]
        z -> z
        where
          findTerm (STerminal _) = True
          findTerm _ = False
          term = S.elemAt (snd x) at
    action1 (st, term)
      | gt <- goto rules cl curst (STerminal curterm)
      , not $ S.null gt
      = [LRShift $ S.findIndex gt stl]
      | otherwise
      = []
      where
        curst = S.elemAt st stl
        curterm = S.elemAt term at
    action2 (st, term)
      = S.toList $ S.map (\e -> LRReduce (pHead e, pBody e)) $
          S.filter (\x@(Point _ p _ b) -> isNothing (b !!! p) && fol curst curterm x) curst
      where
        curst = S.elemAt st stl
        curterm = S.elemAt term at
    goto' (st, nt)
      | gt <- goto rules cl curst (SNonTerminal cursym)
      , not $ S.null gt
      = Just $ S.findIndex gt stl
      | otherwise
      = Nothing
      where
        curst = S.elemAt st stl
        cursym = S.elemAt nt ant

-- makeLR0 :: Rules -> (Int, LRActionTable, LRGotoTable)
makeLR0, makeSLR, makeLR1, makeLALR :: Rules -> Precedence -> Associativity -> (Int, LRActionTable, LRGotoTable)
makeLR0 = makeLR closure (\_ _ _ -> True)

makeSLR rules = makeLR closure (\_ curterm p -> curterm `S.member` follow rules (pHead p)) rules

makeLR1 = makeLR closure1 (\_ curterm p -> curterm `S.member` pLookahead p)

makeLALR rules = makeLR closure (\curst curterm p -> curterm `S.member` getLookahead curst p) rules
  where
    getLookahead curst0 p = S.unions $ S.toList $ S.map pLookahead $ S.filter (\p1 -> stripLA p == stripLA p1) curst1
      where
      curst1 = S.unions $ S.toList $ S.filter (\st -> S.map stripLA curst0 == S.map stripLA st) stl1
    (_, stl1) = states rules closure1

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
    LRShift n : _ -> do
      push [n]
      shift
    LRReduce (nt, b) : _ -> do
      popn $ length b
      t <- peekstack
      when (nt /= StartRule) $ do
        let Just gtt = gotot ! (t, nonTermIx nt)
        push [gtt]
    _ -> return ()
  return $ action ! (x, termIx a)
  where
    terms = allTerminals rules
    nterms = allNonTerminals rules
    nonTermIx x = S.findIndex x nterms
    termIx x = S.findIndex x terms
