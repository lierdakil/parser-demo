{-# LANGUAGE OverloadedStrings #-}
module Dia where

import Data.Tree
import Diagrams
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG

options :: Options SVG V2 Float
options = SVGOptions
    { _size = mkHeight 1024
    , _svgDefinitions = Nothing
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix = "syntax-tree-"
    , _svgAttributes  = []
    , _generateDoctype = False
    }

drawSynTree :: Tree String -> String
drawSynTree t = show $ renderDia SVG options $
  renderTree ((<> circle 1 # fc white) . text)
             (~~)
             (symmLayout' (with & slHSep .~ (4 :: Float) & slVSep .~ 4) t)
  # centerXY # pad 1.1
