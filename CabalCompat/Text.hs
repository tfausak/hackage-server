module CabalCompat.Text
  ( Distribution.Text.Text(disp, parse)
  , Distribution.Pretty.Pretty(pretty)
  , Distribution.Parsec.Class.Parsec(parsec)
  , Distribution.Text.display
  , Distribution.Pretty.prettyShow
  , simpleParse
  ) where

import qualified Distribution.Parsec.Class
import qualified Distribution.Pretty
import qualified Distribution.Text

simpleParse :: Distribution.Parsec.Class.Parsec a => String -> Maybe a
simpleParse = Distribution.Parsec.Class.simpleParsec
