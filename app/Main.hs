{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, MultiWayIf, FlexibleContexts #-}

module Main where

import Graphics.UI.Gtk.WebKit.DOM.Element as E
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as I
import Graphics.UI.Gtk.WebKit.DOM.HTMLTextAreaElement as T
import Graphics.UI.Gtk.WebKit.DOM.HTMLSelectElement as S
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
import Control.Arrow

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
<option value="LR1">LR(1)</option>
<option value="SLR">SLR</option>
</select>
<button id="step" disabled>Step</button>
<button id="run">Run</button>
<table id="input"></table>
<table id="stack"></table>
<table id="table">
</table>
<div id="error">
</div>
<textarea id="grammar" cols="40" rows="10"></textarea>
</body>

</html>
|]

-- initialGrammar :: String
-- initialGrammar = [s|S -> E $
-- E -> E "+" T | T
-- T -> T "*" F | F
-- F -> "(" E ")" | id
-- |]
-- initialGrammar :: String
-- initialGrammar = [s|S -> L "+" R | R
-- L -> "*" R | id
-- R -> L
-- |]
initialGrammar :: String
initialGrammar = [s|E -> T E'
E' -> "+" T E' |
T -> F T'
T' -> "*" F T' |
F -> "(" E ")" | id
|]

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
  I.setValue inputstrel . Just $ "id + id"
  T.setValue grammarel $ Just initialGrammar
  let drawStack how v =
        setInnerHTML stackel $ Just $ concatMap (\i -> "<tr><td>"++ how i++"</td></tr>") v
      drawInput v =
        setInnerHTML inputel $ Just $ "<tr>" ++ concatMap (\i -> "<td>"++ showTerm i++"</td>") v ++ "</tr>"
      updateLRTree v =
        setInnerHTML treeel $ Just $ drawSynForest $ reverse v
      printError v =
        setInnerText errorel $ Just v
      printLRTable rules (action, goto') stcs stnt = do
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
        setInnerHTML tableel $ Just $ h ++ t
      updateLLTree v =
        setInnerHTML treeel $ Just $ drawSynTree $ head $ v $ repeat $ Node "?" []
      printLLTable rules v (i', j') = do
        let ((imin, jmin), (imax, jmax)) = bounds v
            alt = allTerminals rules
            alnt = allNonTerminals rules
            h = wrap "<tr>" "</tr>" $ wrap "<th>" "</th>" "" ++ concatMap (wrap "<th>" "</th>" . showTerm) alt
            t = flip concatMap [imin..imax] $ \i -> wrap "<tr>" "</tr>" $
                  ((wrap "<th>" "</th>" $ showNT $ S.elemAt i alnt) ++) . flip concatMap [jmin..jmax] $ \j ->
                    wrap (if i == i' && j==j' then "<td class='active'>" else "<td>") "</td>" $
                      intercalate "<br>" $ map showRule $ v ! (i,j)
        setInnerHTML tableel $ Just $ h ++ t
  canContinue <- newEmptyMVar
  canRun <- newMVar True
  void $ (stepBtn `on` click) $ liftIO $ void $ tryPutMVar canContinue ()
  void $ (runBtn `on` E.click) $ liftIO $ void $ forkIO $ do
    printError ""
    postGUIAsync $ do
      setAttribute runBtn "disabled" ""
      removeAttribute stepBtn "disabled"
    canRunVal <- tryTakeMVar canRun
    when (isJust canRunVal) $ do
      _ <- tryTakeMVar canContinue
      Just grammar <- postGUISync $ T.getValue grammarel
      case parser grammar of
        Left err -> postGUIAsync $ printError $ show err
        Right rules -> do
          Just inputstrText <- postGUISync $ I.getValue inputstrel
          let inp = map Terminal $ words inputstrText
          if any (`S.notMember` allTerminals rules) inp
          then
            printError "Unknown terminal"
          else do
            let runLR tbl = do
                  let (startSt, action, goto') = tbl rules--makeSLR rules
                      alt = allTerminals rules
                      alnt = allNonTerminals rules
                      run stl = do
                        (stack, input') <- get
                        liftIO $ postGUIAsync $ do
                          drawStack show stack
                          drawInput input'
                          updateLRTree stl
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
                                liftIO $ postGUIAsync $ printLRTable rules (action, goto')
                                  (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
                                  Nothing
                                _ <- liftIO $ takeMVar canContinue
                                run $ c stl
                              [LRReduce (np@(NonTerminal p), als)] -> do
                                (_:ns:_, _) <- get
                                liftIO $ postGUIAsync $ printLRTable rules (action, goto')
                                  (Just (head stack, S.findIndex (head $ input' ++ repeat Eof) alt))
                                  (Just (ns, S.findIndex np alnt))
                                _ <- liftIO $ takeMVar canContinue
                                let (c, r) = splitAt (length als) stl
                                run $ Node p (reverse c) : r
                              [LRReduce (StartRule, _)] -> do
                                _ <- liftIO $ takeMVar canContinue
                                return ()
                              [] -> printError "empty action"
                              _:_:_ ->  printError "ambiguous action"
                  liftIO $ postGUIAsync $ printLRTable rules (action, goto') Nothing Nothing
                  evalStateT (run []) ([startSt], inp ++ [Eof])
                runLL = do
                  let tbl = makeLL1 rules
                      run stl = do
                        (stack, input') <- get
                        liftIO $ postGUIAsync $ do
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
                                liftIO $ postGUIAsync $ printLLTable rules tbl (-1, -1)
                                _ <- liftIO $ takeMVar canContinue
                                run $ stl . c
                              LL1Prod ix (NonTerminal p, als) -> do
                                liftIO $ postGUIAsync $ printLLTable rules tbl ix
                                _ <- liftIO $ takeMVar canContinue
                                run $ stl . uncurry ((:) . Node p) . splitAt (length als)
                              LL1Prod ix (StartRule, _) -> do
                                liftIO $ postGUIAsync $ printLLTable rules tbl ix
                                _ <- liftIO $ takeMVar canContinue
                                run stl
                              LL1Error err -> printError err
                  liftIO $ postGUIAsync $ printLLTable rules tbl (-1, -1)
                  evalStateT (run id) ([start], inp ++ [Eof])

            Just act <- postGUISync $ S.getValue selectel
            case act of
              "LL1" -> runLL
              "LR1" -> runLR makeLR1
              "SLR" -> runLR makeSLR
              x -> printError $ "Unknown: " ++ x
      postGUIAsync $ do
        removeAttribute runBtn "disabled"
        setAttribute stepBtn "disabled" ""
      void $ tryPutMVar canRun True
  where
    start = SNonTerminal StartRule
