module Widget.Languages (
    languagesWidget
) where

import Import

languagesWidget :: Widget
languagesWidget =
    $(widgetFile "widgets/languages")
