module Handler.Home where

import Import
import Util.Handler (title)
import Widget.Languages (languagesWidget)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle $ title "Home"
        $(widgetFile "homepage")
