module Util.User (
    newToken
) where

import ClassyPrelude.Yesod
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

newToken :: IO Text
newToken = pack . toString <$> nextRandom
