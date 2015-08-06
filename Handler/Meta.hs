module Handler.Meta where

import Import
import Util.Handler (title)


getMetaAboutR :: Handler Html
getMetaAboutR =
    defaultLayout $ do
        setTitle $ title "About glot.io"
        $(widgetFile "meta/about")

getMetaApiDocsR :: Handler Html
getMetaApiDocsR =
    defaultLayout $ do
        setTitle $ title "Api Docs"
        $(widgetFile "meta/api-docs")
