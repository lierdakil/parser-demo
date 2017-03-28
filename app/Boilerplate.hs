module Boilerplate where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.EventM
import Control.Monad.Trans

import Util

mainWidget :: String -> (Document -> IO ()) -> IO ()
mainWidget s f = do
  Just doc <- currentDocument

  -- once the page is loaded, install event handlers as needed
  f doc
  return ()

-- getElem :: (DocumentClass self, MonadIO m) => self -> String -> m Element
getElem doc n = liftIO $ getElementById doc n >>= fromMaybeM ("no element " ++ n)
