module CabalCompat.Text
  ( Distribution.Text.Text(disp, parse)
  , Distribution.Pretty.Pretty(pretty)
  , Distribution.Parsec.Class.Parsec(parsec)
  , Distribution.Text.display
  , Distribution.Pretty.prettyShow
  , Distribution.Text.simpleParse
  , Distribution.Parsec.Class.simpleParsec
  ) where

import qualified Distribution.Parsec.Class
import qualified Distribution.Pretty
import qualified Distribution.Text
