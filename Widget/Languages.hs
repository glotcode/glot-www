module Widget.Languages (
    languagesWidget
) where

import Import
import qualified Glot.Language

languagesWidget :: [Glot.Language.LanguageConfig] -> Widget
languagesWidget languageConfigs =
    $(widgetFile "widgets/languages")
