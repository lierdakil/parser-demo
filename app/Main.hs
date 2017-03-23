{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Data.Maybe
import Control.Monad.IO.Class

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.Types
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.DOM.MouseEvent
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement as H
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.WebKit.DOM.HTMLDocument
import Graphics.UI.Gtk.WebKit.DOM.EventTarget
import Graphics.UI.Gtk.WebKit.DOM.EventTargetClosures
import QQ

fromMaybeM :: Monad m => String -> Maybe a -> m a
fromMaybeM st = maybe (fail st) return

main :: IO ()
main = do
  -- initialize Gtk
  _ <- initGUI

  -- create a new window, a scrolled window, and a new webview
  w  <- windowNew
  sw <- scrolledWindowNew Nothing Nothing
  wv <- webViewNew

  -- set the child of the parent to the scrolled window,
  -- and set some others attributes
  set w
    [ containerChild       := sw
    , windowDefaultWidth   := 1000
    , windowDefaultHeight  := 800
    , containerBorderWidth := 0
    ]
  -- set the child of the scrolled windows to the webview.
  set sw [ containerChild := wv ]

  -- load our HTML string on the webview.
  webViewLoadString wv [s|
<div id="abc">
  <h1>Hello (with click handler)</h1>
</div>
<div id="def">
  <h1>Hello (without handler)</h1>
</div>
|] (Just "text/html") ""

  -- on destroying event, we quit the mainloop
  -- onDestroy w mainQuit
  w `on` deleteEvent $ liftIO mainQuit >> return False
  -- show all widgets starting from the root window
  widgetShowAll w

  -- once the page is loaded, install event handlers as needed
  wv `on` documentLoadFinished $ \wf -> do
    doc <- webViewGetDomDocument wv >>= fromMaybeM "no document"
    abc <- getElementById doc "abc" >>= fromMaybeM "no element id=abc found"
    evlstn <- eventListenerNew $ \(e :: MouseEvent) -> do
      x <- getClientX e
      y <- getClientY e
      putStrLn $ "evlist called at (" ++ show x ++ ", " ++ show y ++ ")"
    putStrLn "Installing handler"
    addEventListener abc "click" (Just evlstn) True
  -- start GTK main loop
  mainGUI
