module Handler.Meta where

import Import
import Util.Handler (title)
import qualified Util.Handler as Handler


getMetaAboutR :: Handler Html
getMetaAboutR =
    defaultLayout $ do
        setTitle $ title "About"
        Handler.setCanonicalUrl MetaAboutR
        $(widgetFile "meta/about")

getMetaApiDocsR :: Handler Html
getMetaApiDocsR =
    defaultLayout $ do
        setTitle $ title "Api Docs"
        Handler.setCanonicalUrl MetaApiDocsR
        $(widgetFile "meta/api-docs")
