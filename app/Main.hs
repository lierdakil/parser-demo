{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, MultiWayIf, FlexibleContexts, ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Graphics.UI.Gtk.WebKit.DOM.Document as D hiding (click)
import Graphics.UI.Gtk.WebKit.DOM.Element as E
import Graphics.UI.Gtk.WebKit.DOM.Node as N
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as I
import Graphics.UI.Gtk.WebKit.DOM.HTMLTextAreaElement as T
import Graphics.UI.Gtk.WebKit.DOM.HTMLSelectElement as S
import Graphics.UI.Gtk.WebKit.DOM.HTMLOptionElement as O
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement as H hiding (click)
import QQ
import Control.Monad.Reader
import Graphics.UI.Gtk.WebKit.DOM.EventM


import Boilerplate
import LL
import LR
import Control.Monad.State
import qualified Data.Set as S
import Data.Tree
import Dia
import Data.Maybe
import Data.Array
import Data.List
import Data.IORef

initialContent :: String
initialContent = [s|
<!DOCTYPE html>
<html>
<head>
<title>Title of the document</title>
<style>
#tree svg {
  float: right;
  max-height: 100vh;
  max-width: 40vw;
}
#stack {
  float: left;
}
table {
  border: 1px solid;
  border-spacing: 0;
  min-width: 1em;
  min-height: 1em;
}
table td {
  border: 1px solid;
  white-space: pre;
  text-align: center;
  padding: 0.5ex;
}
#table {
  margin-left: 3em;
  margin-top: 2em;
}
textarea {
  margin-left: 3em;
  display: block;
}
#sample {
  display: block;
  margin-top: 2em;
  margin-left: 3em;
}
#input {
  margin-left: 3em;
}
#stack {
  margin-top: 2em;
}
#table td.active {
  background: green;
}
</style>
</head>

<body>
<div id="tree"></div>
<input type="text" id="inputstr">
<select id="select">
<option selected value="LL1">LL(1)</option>
<option value="LR0">LR(0)</option>
<option value="SLR">SLR</option>
<option value="LR1">LR(1)</option>
<option value="LALR">LALR</option>
</select>
<button id="stepback" disabled>&lt;-</button>
<button id="step" disabled>-&gt;</button>
<button id="run">Run</button>
<table id="input"></table>
<table id="stack"></table>
<table id="table">
</table>
<div id="error">
</div>
<select id="sample"></select>
<textarea id="grammar" cols="40" rows="10"></textarea>
</body>

</html>
|]

samples :: [(String, String)]
samples = [
    (,) "id + ( id + id ) * id" [s|E -> E "+" T | T
T -> T "*" F | F
F -> "(" E ")" | id
|]
  , (,) "id + ( id + id ) * id" [s|E -> T E'
E' -> "+" T E' |
T -> F T'
T' -> "*" F T' |
F -> "(" E ")" | id
|]
  , (,) "id = * id" [s|S -> L "=" R | R
L -> "*" R | id
R -> L
|]
  , (,) "b c d" [s|S -> a A d | b B d | a B e | b A e
A -> c
B -> c
|]
  , (,) "id + id - id + id" [s|E -> E "^" E | E "*" E | E "/" E | E "+" E | E "-" E | "(" E ")" | id

"^"
"*" "/"
"+" "-"

Right "^"
|]
  ]

wrap :: [a] -> [a] -> [a] -> [a]
wrap b e st = b ++ st ++ e

main = mainWidget initialContent $ \doc -> do
  stepBtn <- getElem doc "step"
  stepbackBtn <- getElem doc "stepback"
  runBtn <- getElem doc "run"
  inputstrel <- castToHTMLInputElement <$> getElem doc "inputstr"
  stackel <- castToHTMLElement <$> getElem doc "stack"
  inputel <- castToHTMLElement <$> getElem doc "input"
  grammarel <- castToHTMLTextAreaElement <$> getElem doc "grammar"
  treeel <- getElem doc "tree"
  tableel <- getElem doc "table"
  errorel <- castToHTMLElement <$> getElem doc "error"
  selectel <- castToHTMLSelectElement <$> getElem doc "select"
  sampleel <- castToHTMLSelectElement <$> getElem doc "sample"
  forM_ [0 .. length samples - 1] $ \i -> do
    Just opt <- fmap castToHTMLOptionElement <$> D.createElement doc (Just "option")
    H.setInnerText opt (Just $ "Example " ++ show i)
    O.setValue opt $ show i
    appendChild sampleel (Just opt)
  let (initin, initgr) = head samples
  I.setValue inputstrel . Just $ initin
  T.setValue grammarel $ Just initgr
  void $ (sampleel `on` E.change) $ liftIO $ do
    n <- S.getSelectedIndex sampleel
    let (initin', initgr') = samples !! n
    I.setValue inputstrel . Just $ initin'
    T.setValue grammarel $ Just initgr'
  let ?drawStackSym = void .
        setInnerHTML stackel . Just . concatMap (\i -> "<tr><td>"++ showSym i ++"</td></tr>")
      ?drawStackShow = void .
        setInnerHTML stackel . Just . concatMap (\i -> "<tr><td>"++ show i ++"</td></tr>")
      ?drawInput = \v ->
        setInnerHTML inputel $ Just $ "<tr>" ++ concatMap (\i -> "<td>"++ showTerm i++"</td>") v ++ "</tr>"
      ?updateLRTree = setInnerHTML treeel . Just . drawSynForest . reverse
      ?printError = setInnerText errorel . Just
      ?printLRTable = \rules (action, goto') stcs stnt -> do
        let ((imin, jmin), (imax, jmax)) = bounds action
            ((_, jmin'), (_, jmax')) = bounds goto'
            alt = allTerminals rules
            alnt = allNonTerminals rules
            (i', j') = fromMaybe (-1, -1) stcs
            (i'', j'') = fromMaybe (-1, -1) stnt
            h = wrap "<tr>" "</tr>" $ wrap "<th>" "</th>" "" ++ concatMap (wrap "<th>" "</th>" . showTerm) alt ++ concatMap (wrap "<th>" "</th>" . showNT) alnt
            t = flip concatMap [imin..imax] $ \i -> wrap "<tr>" "</tr>" $
                  (((wrap "<th>" "</th>" $ show i) ++) . flip concatMap [jmin..jmax] $ \j ->
                    wrap (if i == i' && j==j' then "<td class='active'>" else "<td>") "</td>" $
                      intercalate "<br>" $ map showAction $ action ! (i,j))
                  ++
                  concatMap (\j ->
                    wrap (if i == i'' && j==j'' then "<td class='active'>" else "<td>") "</td>" $
                      maybe "" show $ goto' ! (i,j))
                    [jmin'..jmax']
            showAction (LRShift n) = show n
            showAction (LRReduce x) = showRule x
            showAction LRAccept = "accept"
        setInnerHTML tableel $ Just $ h ++ t
      ?updateLLTree = \v -> liftIO $
        setInnerHTML treeel $ Just $ drawSynTree $ head $ v $ repeat $ Node "?" []
      ?printLLTable = \rules v (i', j') -> liftIO $ do
        let ((imin, jmin), (imax, jmax)) = bounds v
            alt = allTerminals rules
            alnt = allNonTerminals rules
            h = wrap "<tr>" "</tr>" $ wrap "<th>" "</th>" "" ++ concatMap (wrap "<th>" "</th>" . showTerm) alt
            t = flip concatMap [imin..imax] $ \i -> wrap "<tr>" "</tr>" $
                  ((wrap "<th>" "</th>" $ showNT $ S.elemAt i alnt) ++) . flip concatMap [jmin..jmax] $ \j ->
                    wrap (if i == i' && j==j' then "<td class='active'>" else "<td>") "</td>" $
                      intercalate "<br>" $ map showRule $ v ! (i,j)
        setInnerHTML tableel $ Just $ h ++ t
  let readGrammar = T.getValue grammarel
      readInput =  I.getValue inputstrel
      readSelectedParser = S.getValue selectel
      enableStep =  do
        removeAttribute stepBtn "disabled"
        removeAttribute stepbackBtn "disabled"
  results <- newIORef []
  curstep <- newIORef 0
  void $ (stepBtn `on` click) $ liftIO $ void $ do
    xs <- readIORef results
    st <- readIORef curstep
    when (st + 1 < length xs) $ do
      xs !! (st + 1)
      writeIORef curstep (st + 1)
  void $ (stepbackBtn `on` click) $ liftIO $ void $ do
    xs <- readIORef results
    st <- readIORef curstep
    when (st > 0) $ do
      xs !! (st - 1)
      writeIORef curstep (st - 1)
  void $ (runBtn `on` E.click) $ liftIO $ void $ do
    ?printError ""
    Just grammar <- readGrammar
    case parser grammar of
      Left err -> ?printError $ show err
      Right (rules, prio, assoc) -> do
        Just inputstrText <- readInput
        let inp = map Terminal $ words inputstrText
        if any (`S.notMember` allTerminals rules) inp
        then
          ?printError "Unknown terminal"
        else do
          Just act <- readSelectedParser
          res <- case act of
            "LL1" -> runLL rules inp
            "LR1" -> runLR makeLR1 rules prio assoc inp
            "LR0" -> runLR makeLR0 rules prio assoc inp
            "SLR" -> runLR makeSLR rules prio assoc inp
            "LALR" -> runLR makeLALR rules prio assoc inp
            x -> ?printError ("Unknown: " ++ x) >> return []
          writeIORef results $ reverse res
          writeIORef curstep (-1)
          enableStep

runLR tbl rules prio assoc inp = do
  ?printLRTable rules (action, goto') Nothing Nothing
  evalStateT (run [] []) ([startSt], inp ++ [Eof])
  where
    (startSt, action, goto') = tbl rules prio assoc
    alt = allTerminals rules
    alnt = allNonTerminals rules
    run stl acts = do
      (stack, input') <- get
      let doone :: IO ()
          doone = do
            void $ ?drawStackShow stack
            void $ ?drawInput input'
            void $ ?updateLRTree stl
            ?printLRTable rules (action, goto')
              (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
              Nothing
      if
        | null stack && null input' -> return (doone:acts)
        | null stack -> liftIO $ ?printError "empty stack" >> return (doone:acts)
        | otherwise -> do
          sym' <- stepLR rules action goto'
          case sym' of
            [LRShift _] -> do
              let c =
                    case head input' of
                      Terminal term -> (Node term [] :)
                      Epsilon -> (Node "" [] :)
                      Eof -> id
              run (c stl) (doone:acts)
            [LRReduce (np@(NonTerminal p), als)] -> do
              (_:ns:_, _) <- get
              let dotwo = do
                    doone
                    ?printLRTable rules (action, goto')
                      (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
                      (Just (ns, S.findIndex np alnt))
              let (c, r) = splitAt (length als) stl
              run (Node p (reverse c) : r) (dotwo:acts)
            [LRReduce (StartRule, _)] -> return (doone:acts)
            [LRAccept] -> return (doone:acts)
            [] -> liftIO $ ?printError "empty action" >> return (doone:acts)
            _:_:_ ->  liftIO $ ?printError "ambiguous action" >> return (doone:acts)

runLL rules inp = do
  ?printLLTable rules tbl (-1, -1)
  evalStateT (run id []) ([start], inp ++ [Eof])
  where
    start = SNonTerminal StartRule
    tbl = makeLL1 rules
    run stl act = do
      (stack, input') <- get
      let doone :: IO ()
          doone = do
            void $ ?drawStackSym stack
            void $ ?drawInput input'
            ?updateLLTree stl
      if
        | null stack && null input' -> return (doone:act)
        | null stack -> liftIO (?printError "empty stack") >> return (doone:act)
        | null input' -> liftIO (?printError "empty input") >> return (doone:act)
        | otherwise -> do
          sym' <- stepLL1FA rules tbl
          case sym' of
            LL1Shift sym -> do
              let c =
                    case sym of
                      Terminal term -> (Node term [] :)
                      Epsilon -> (Node "" [] :)
                      Eof -> id
              let dotwo = doone >> ?printLLTable rules tbl (-1, -1)
              run (stl . c) (dotwo:act)
            LL1Prod ix (NonTerminal p, als) -> do
              let dotwo = doone >> ?printLLTable rules tbl ix
              run (stl . uncurry ((:) . Node p) . splitAt (length als)) (dotwo:act)
            LL1Prod ix (StartRule, _) -> do
              let dotwo = doone >> ?printLLTable rules tbl ix
              run stl (dotwo:act)
            LL1Error err -> liftIO (?printError err) >> return (doone:act)
