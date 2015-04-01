module Widget.RunResult (
    runResultWidget
) where

import Import

runResultWidget :: Widget
runResultWidget = $(widgetFile "widgets/run-result")
