module Handler.Home where

import Import
import Util.Handler (title)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle $ title "Home - glot.io"
        $(widgetFile "homepage")
