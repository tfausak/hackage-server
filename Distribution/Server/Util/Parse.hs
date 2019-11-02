-- | Parsing and UTF8 utilities
module Distribution.Server.Util.Parse (
    Parse.int, unpackUTF8, packUTF8
  ) where

import qualified CabalCompat.ReadP as Parse

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy           as Text
import qualified Data.Text.Lazy.Encoding  as Text
import qualified Data.Text.Encoding.Error as Text

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input        
--                                                                               
-- (Also in Distribution.Simple.Utils, but not exported)                         
ignoreBOM :: String -> String                                                    
ignoreBOM ('\xFEFF':string) = string   
ignoreBOM string            = string                                             

unpackUTF8 :: ByteString -> String
unpackUTF8 = ignoreBOM . Text.unpack . Text.decodeUtf8With Text.lenientDecode

packUTF8 :: String -> ByteString
packUTF8 = Text.encodeUtf8 . Text.pack
