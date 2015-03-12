module Handler.Language where

import Import
import Widget

getLanguageR :: Language -> Handler Html
getLanguageR lang =
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "language")


