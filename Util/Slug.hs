module Util.Slug (
    mkSlug
) where

import ClassyPrelude.Yesod
import Text.Regex (subRegex, mkRegex)

mkSlug :: Text -> Text
mkSlug t = pack . replaceDotPrefix . replaceHyphenPrefix . ensureSingleDotHypen . ensureSingleDot . ensureSingleHypen . replaceDisallowed . unpack $ toLower t

replaceDotPrefix :: String -> String
replaceDotPrefix = replace' "^\\." "dot"

replaceHyphenPrefix :: String -> String
replaceHyphenPrefix = replace' "^-" "hyphen"

ensureSingleDotHypen :: String -> String
ensureSingleDotHypen = replace' "[-.]{2,}" "-"

ensureSingleDot :: String -> String
ensureSingleDot = replace' "\\.{2,}" "."

ensureSingleHypen :: String -> String
ensureSingleHypen = replace' "-{2,}" "-"

replaceDisallowed :: String -> String
replaceDisallowed = replace' "[^a-z0-9.-]+" "-"

replace' :: String -> String -> String -> String
replace' pattern replacement str = subRegex (mkRegex pattern) str replacement
