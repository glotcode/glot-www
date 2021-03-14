module Widget.Languages (
    languagesWidget
) where

import Import
import qualified Glot.Language as Language

languagesWidget :: [Language.Language] -> Widget
languagesWidget languages =
    $(widgetFile "widgets/languages")
