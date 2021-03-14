module Widget.Languages (
    languagesWidget
) where

import Import
import qualified Glot.Language

languagesWidget :: [Glot.Language.Language] -> Widget
languagesWidget languages =
    $(widgetFile "widgets/languages")
