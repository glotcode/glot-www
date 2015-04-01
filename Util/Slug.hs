module Util.Slug (
    mkSlug
) where

import ClassyPrelude.Yesod
import Text.Regex (subRegex, mkRegex)

mkSlug :: Text -> Text
mkSlug t = pack . replaceWithHyphen . removeNonEnglish . unpack $ toLower t

ensureSingleHypen :: String -> String
ensureSingleHypen str = subRegex (mkRegex "-{2,}") str "-"

replaceWithHyphen :: String -> String
replaceWithHyphen str = ensureSingleHypen $ subRegex (mkRegex "[ _]") str "-"

removeNonEnglish :: String -> String
removeNonEnglish str = subRegex (mkRegex "[^a-z0-9_ -]+") str "-"
