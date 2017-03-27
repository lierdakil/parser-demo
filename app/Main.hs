{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, MultiWayIf, FlexibleContexts #-}

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
import Control.Concurrent.MVar
import Control.Concurrent
import Dia
import Data.Maybe
import Graphics.UI.Gtk (postGUIAsync, postGUISync)
import Data.Array
import Data.List

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
<button id="step" disabled>Step</button>
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
    (,) "id = * id" [s|S -> L "=" R | R
L -> "*" R | id
R -> L
|]
  , (,) "b c d" [s|S -> a A d | b B d | a B e | b A e
A -> c
B -> c
|]
  , (,) "id + ( id + id ) * id" [s|E -> E "+" T | T
T -> T "*" F | F
F -> "(" E ")" | id
|]
  , (,) "id + ( id + id ) * id" [s|E -> T E'
E' -> "+" T E' |
T -> F T'
T' -> "*" F T' |
F -> "(" E ")" | id
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

main :: IO ()
main = mainWidget initialContent $ \doc -> do
  stepBtn <- getElem doc "step"
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
    let (initin, initgr) = samples !! n
    I.setValue inputstrel . Just $ initin
    T.setValue grammarel $ Just initgr
  let drawStack how v = liftIO $ postGUIAsync $
        setInnerHTML stackel $ Just $ concatMap (\i -> "<tr><td>"++ how i++"</td></tr>") v
      drawInput v = liftIO $ postGUIAsync $
        setInnerHTML inputel $ Just $ "<tr>" ++ concatMap (\i -> "<td>"++ showTerm i++"</td>") v ++ "</tr>"
      updateLRTree v = liftIO $ postGUIAsync $
        setInnerHTML treeel $ Just $ drawSynForest $ reverse v
      printError v = liftIO $ postGUIAsync $
        setInnerText errorel $ Just v
      printLRTable rules (action, goto') stcs stnt =  liftIO $ postGUIAsync $ do
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
      updateLLTree v = liftIO $ postGUIAsync $
        setInnerHTML treeel $ Just $ drawSynTree $ head $ v $ repeat $ Node "?" []
      printLLTable rules v (i', j') = liftIO $ postGUIAsync $ do
        let ((imin, jmin), (imax, jmax)) = bounds v
            alt = allTerminals rules
            alnt = allNonTerminals rules
            h = wrap "<tr>" "</tr>" $ wrap "<th>" "</th>" "" ++ concatMap (wrap "<th>" "</th>" . showTerm) alt
            t = flip concatMap [imin..imax] $ \i -> wrap "<tr>" "</tr>" $
                  ((wrap "<th>" "</th>" $ showNT $ S.elemAt i alnt) ++) . flip concatMap [jmin..jmax] $ \j ->
                    wrap (if i == i' && j==j' then "<td class='active'>" else "<td>") "</td>" $
                      intercalate "<br>" $ map showRule $ v ! (i,j)
        setInnerHTML tableel $ Just $ h ++ t
      disableRun = postGUIAsync $ do
        setAttribute runBtn "disabled" ""
        removeAttribute stepBtn "disabled"
      readGrammar = postGUISync $ T.getValue grammarel
      readInput =  postGUISync $ I.getValue inputstrel
      readSelectedParser = postGUISync $ S.getValue selectel
      enableRun = postGUIAsync $ do
        removeAttribute runBtn "disabled"
        setAttribute stepBtn "disabled" ""
  canContinue <- newEmptyMVar
  canRun <- newMVar True
  void $ (stepBtn `on` click) $ liftIO $ void $ tryPutMVar canContinue ()
  void $ (runBtn `on` E.click) $ liftIO $ void $ forkIO $ do
    printError ""
    disableRun
    canRunVal <- tryTakeMVar canRun
    when (isJust canRunVal) $ do
      _ <- tryTakeMVar canContinue
      Just grammar <- readGrammar
      case parser grammar of
        Left err -> printError $ show err
        Right (rules, prio, assoc) -> do
          Just inputstrText <- readInput
          let inp = map Terminal $ words inputstrText
          if any (`S.notMember` allTerminals rules) inp
          then
            printError "Unknown terminal"
          else do
            let runLR tbl = do
                  let (startSt, action, goto') = tbl rules prio assoc
                      alt = allTerminals rules
                      alnt = allNonTerminals rules
                      run stl = do
                        (stack, input') <- get
                        drawStack show stack
                        drawInput input'
                        updateLRTree stl
                        printLRTable rules (action, goto')
                          (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
                          Nothing
                        if
                          | null stack && null input' -> return ()
                          | null stack -> liftIO (printError "empty stack")
                          | otherwise -> do
                            sym' <- stepLR rules action goto'
                            case sym' of
                              [LRShift _] -> do
                                let c =
                                      case head input' of
                                        Terminal term -> (Node term [] :)
                                        Epsilon -> (Node "" [] :)
                                        Eof -> id
                                _ <- liftIO $ takeMVar canContinue
                                run $ c stl
                              [LRReduce (np@(NonTerminal p), als)] -> do
                                (_:ns:_, _) <- get
                                printLRTable rules (action, goto')
                                  (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
                                  (Just (ns, S.findIndex np alnt))
                                _ <- liftIO $ takeMVar canContinue
                                let (c, r) = splitAt (length als) stl
                                run $ Node p (reverse c) : r
                              [LRReduce (StartRule, _)] -> return ()
                              [LRAccept] -> return ()
                              [] -> printError "empty action"
                              _:_:_ ->  printError "ambiguous action"
                  printLRTable rules (action, goto') Nothing Nothing
                  evalStateT (run []) ([startSt], inp ++ [Eof])
                runLL = do
                  let tbl = makeLL1 rules
                      run stl = do
                        (stack, input') <- get
                        drawStack showSym stack
                        drawInput input'
                        updateLLTree stl
                        if
                          | null stack && null input' -> return ()
                          | null stack -> liftIO (printError "empty stack")
                          | null input' -> liftIO (printError "empty input")
                          | otherwise -> do
                            sym' <- stepLL1FA rules tbl
                            case sym' of
                              LL1Shift sym -> do
                                let c =
                                      case sym of
                                        Terminal term -> (Node term [] :)
                                        Epsilon -> (Node "" [] :)
                                        Eof -> id
                                printLLTable rules tbl (-1, -1)
                                _ <- liftIO $ takeMVar canContinue
                                run $ stl . c
                              LL1Prod ix (NonTerminal p, als) -> do
                                printLLTable rules tbl ix
                                _ <- liftIO $ takeMVar canContinue
                                run $ stl . uncurry ((:) . Node p) . splitAt (length als)
                              LL1Prod ix (StartRule, _) -> do
                                printLLTable rules tbl ix
                                _ <- liftIO $ takeMVar canContinue
                                run stl
                              LL1Error err -> printError err
                  printLLTable rules tbl (-1, -1)
                  evalStateT (run id) ([start], inp ++ [Eof])

            Just act <- readSelectedParser
            case act of
              "LL1" -> runLL
              "LR1" -> runLR makeLR1
              "LR0" -> runLR makeLR0
              "SLR" -> runLR makeSLR
              "LALR" -> runLR makeLALR
              x -> printError $ "Unknown: " ++ x
      enableRun
      void $ tryPutMVar canRun True
  where
    start = SNonTerminal StartRule
