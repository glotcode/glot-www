module Util (
    sha1sum
) where

import ClassyPrelude.Yesod hiding (unpack, hash)
import Text.Printf (printf)
import Data.ByteString (unpack)
import Crypto.Hash.SHA1 (hash)

sha1sum :: Text -> Text
sha1sum content = pack $ unpack (hash $ encodeUtf8 $ content) >>= printf "%02x"
