module Handler.Home where

import Import
import Widget


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "glot.io - home"
        $(widgetFile "homepage")
