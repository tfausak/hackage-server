module CabalCompat.Text
  ( Distribution.Text.Text(disp, parse)
  , Distribution.Pretty.Pretty(pretty)
  , Distribution.Parsec.Class.Parsec(parsec)
  , display
  , simpleParse
  ) where

import qualified Distribution.Parsec.Class
import qualified Distribution.Pretty
import qualified Distribution.Text

display :: Distribution.Pretty.Pretty a => a -> String
display = Distribution.Pretty.prettyShow

simpleParse :: Distribution.Parsec.Class.Parsec a => String -> Maybe a
simpleParse = Distribution.Parsec.Class.simpleParsec
