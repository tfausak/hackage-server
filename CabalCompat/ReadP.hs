module CabalCompat.ReadP
  ( Distribution.Parsec.Class.CabalParsing
  , Distribution.Compat.CharParsing.anyChar
  , Distribution.Compat.CharParsing.char
  , Distribution.Compat.CharParsing.count
  , int
  , Distribution.Compat.CharParsing.many
  , Distribution.Compat.CharParsing.munch
  , Distribution.Compat.CharParsing.munch1
  , parseCommaList
  , parseMaybeQuoted
  , pfail
  , readPTime'
  , Distribution.Compat.CharParsing.satisfy
  , Distribution.Compat.CharParsing.sepBy1
  , skipSpaces
  , (<++)
  ) where

import qualified Control.Monad
import qualified Data.Char
import qualified Data.Time
import qualified Distribution.Compat.CharParsing
import qualified Distribution.Parsec.Class

int :: Distribution.Parsec.Class.CabalParsing m => m Int
int = do
  first <- Distribution.Compat.CharParsing.satisfy Data.Char.isDigit
  if first == '0'
    then pure 0
    else do
      rest <- Distribution.Compat.CharParsing.munch Data.Char.isDigit
      pure . read $ first : rest

parseCommaList :: Distribution.Parsec.Class.CabalParsing m => m a -> m [a]
parseCommaList p = Distribution.Compat.CharParsing.sepBy p $ do
  skipSpaces
  Control.Monad.void $ Distribution.Compat.CharParsing.char ','
  skipSpaces

parseMaybeQuoted :: Distribution.Parsec.Class.CabalParsing m => m a -> m a
parseMaybeQuoted p = Control.Monad.mplus (parseQuoted p) p

parseQuoted :: Distribution.Parsec.Class.CabalParsing m => m a -> m a
parseQuoted = Distribution.Compat.CharParsing.between
  (Distribution.Compat.CharParsing.char '"')
  (Distribution.Compat.CharParsing.char '"')

pfail :: Distribution.Parsec.Class.CabalParsing m => m a
pfail = Control.Monad.mzero

readPTime' :: (Distribution.Parsec.Class.CabalParsing m, Data.Time.ParseTime t) => String -> m t
readPTime' format = case format of
  "%c" -> do
    input <- Distribution.Compat.CharParsing.count
      (length "Fri Nov  1 23:05:27 UTC 2019")
      Distribution.Compat.CharParsing.anyChar
    Data.Time.parseTimeM False Data.Time.defaultTimeLocale format input
  _ -> fail $ "readPTime': unsupported format: " <> show format

skipSpaces :: Distribution.Parsec.Class.CabalParsing m => m ()
skipSpaces = Distribution.Compat.CharParsing.spaces

(<++) :: Distribution.Parsec.Class.CabalParsing m => m a -> m a -> m a
(<++) = Control.Monad.mplus
