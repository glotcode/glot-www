module Util (
    iso8601Format,
    visibilityFormat
) where

import ClassyPrelude.Yesod
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)

utcFormat :: UTCTime -> Text
utcFormat time = pack $ formatTime defaultTimeLocale "%c" time

iso8601Format :: Text -> Text
iso8601Format time = utcFormat $ fromJust $ parseISO8601 $ unpack time

visibilityFormat :: Bool -> Text
visibilityFormat True = "Public"
visibilityFormat False = "Secret"
