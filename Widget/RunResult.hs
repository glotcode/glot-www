module Widget.RunResult (
    runResultWidget
) where

import Import

runResultWidget :: Maybe (Entity RunResult) -> Widget
runResultWidget mRunResult = $(widgetFile "widgets/run-result")
