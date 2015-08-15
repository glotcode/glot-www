module Widget.Languages (
    languagesWidget
) where

import Import

languagesWidget :: Maybe Text -> Widget
languagesWidget mTitle =
    $(widgetFile "widgets/languages")
