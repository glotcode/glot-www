module Handler.Api where

import Import
import Util.Handler (title)

getApiR :: Handler Html
getApiR =
    defaultLayout $ do
        setTitle $ title "Api"
        $(widgetFile "api")
