module Widget.Share (
    shareWidget
) where

import Import

shareWidget :: Text  -> Widget
shareWidget snippetId = $(widgetFile "widgets/share")
