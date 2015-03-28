module Handler.Compose where

import Import
import Widget

getComposeR :: Language -> Handler Html
getComposeR lang =
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "language")


