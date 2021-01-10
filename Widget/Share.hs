module Widget.Share (
    shareWidget
) where

import Import

shareWidget :: Text  -> Widget
shareWidget slug =
    $(widgetFile "widgets/share")
