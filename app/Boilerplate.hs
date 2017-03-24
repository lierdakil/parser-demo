module Boilerplate where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.Types
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.DOM.Document
import Control.Monad.Trans

import Util

mainWidget :: String -> (Document -> IO ()) -> IO ()
mainWidget s f = do
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

  webViewLoadString wv s (Just "text/html") ""

  -- on destroying event, we quit the mainloop
  -- onDestroy w mainQuit
  _ <- w `on` deleteEvent $ liftIO mainQuit >> return False
  -- show all widgets starting from the root window
  widgetShowAll w

  -- once the page is loaded, install event handlers as needed
  _ <- wv `on` documentLoadFinished $ \_ -> liftIO (webViewGetDomDocument wv) >>= fromMaybeM "no document" >>= f
  mainGUI

getElem :: (DocumentClass self, MonadIO m) => self -> String -> m Element
getElem doc n = liftIO $ getElementById doc n >>= fromMaybeM ("no element " ++ n)
