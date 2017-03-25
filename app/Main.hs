{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, MultiWayIf, FlexibleContexts #-}

module Main where

import Graphics.UI.Gtk.WebKit.DOM.Element as E
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as I
import Graphics.UI.Gtk.WebKit.DOM.HTMLTextAreaElement as T
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement as H hiding (click)
import QQ
import Control.Monad.Reader
import Graphics.UI.Gtk.WebKit.DOM.EventM


import Boilerplate
import Lib
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

initialGrammar :: String
initialGrammar = [s|S -> E $
E -> T E'
E' -> "+" T E' | ε
T -> F T'
T' -> "*" F T' | ε
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
  I.setValue inputstrel . Just $ "id + id"
  T.setValue grammarel $ Just initialGrammar
  let drawStack v =
        setInnerHTML stackel $ Just $ concatMap (\i -> "<tr><td>"++ showSym i++"</td></tr>") v
      drawInput v =
        setInnerHTML inputel $ Just $ "<tr>" ++ concatMap (\i -> "<td>"++ showTerm i++"</td>") v ++ "</tr>"
      updateTree v =
        setInnerHTML treeel $ Just $ drawSynTree $ head $ v $ repeat $ Node "?" []
      printError v =
        setInnerText errorel $ Just v
      printTable rules v i' j' = do
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
            let tbl = makeLL1 rules
                run stl = do
                  (stack, input') <- get
                  liftIO $ postGUIAsync $ do
                    drawStack stack
                    drawInput input'
                    updateTree stl
                  if
                    | null stack && null input' -> return ()
                    | null stack -> liftIO (printError "empty stack")
                    | null input' -> liftIO (printError "empty input")
                    | otherwise -> do
                      sym' <- stepLL1FA rules tbl
                      case sym' of
                        Right sym -> do
                          let n =
                                case sym of
                                  Right (_, (_, r)) -> length r
                                  _ -> 0
                              c =
                                case sym of
                                  Left (Terminal term) -> Node term
                                  Left Epsilon -> Node "ε"
                                  Left Eof -> Node "$"
                                  Right (_, (NonTerminal nonTerm, _)) -> Node nonTerm
                          case sym of
                            Right ((i, j), _) -> liftIO $ postGUIAsync $ printTable rules tbl i j
                            _ -> liftIO $ postGUIAsync $ printTable rules tbl (-1) (-1)
                          _ <- liftIO $ takeMVar canContinue
                          run $ stl . uncurry ((:) . c) . splitAt n
                        Left err -> printError err
            liftIO $ postGUIAsync $ printTable rules tbl (-1) (-1)
            evalStateT (run id) ([start], inp ++ [Eof])
      postGUIAsync $ do
        removeAttribute runBtn "disabled"
        setAttribute stepBtn "disabled" ""
      void $ tryPutMVar canRun True
  where
    start = SNonTerminal $ NonTerminal "S"
