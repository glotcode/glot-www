module Util.Hash (
    sha1,
    sha1Lazy,
    sha1Text
) where

import ClassyPrelude.Yesod hiding (unpack, hash)
import Text.Printf (printf)
import Data.ByteString (unpack)
import qualified Data.ByteString.Lazy as L
import Crypto.Hash.SHA1 (hash, hashlazy)

sha1 :: ByteString -> Text
sha1 bytes = pack $ unpack (hash bytes) >>= printf "%02x"

sha1Lazy :: L.ByteString -> Text
sha1Lazy bytes = pack $ unpack (hashlazy bytes) >>= printf "%02x"

sha1Text :: Text -> Text
sha1Text = sha1 . encodeUtf8
