module Handler.Snippet where

import Prelude (read)
import Import
import Widget
import Model.Snippet.Api (getSnippet)

getSnippetR :: Text -> Handler Html
getSnippetR snippetId = do
    snippet <- liftIO $ getSnippet snippetId Nothing
    let lang = read $ unpack $ snippetLanguage snippet :: Language
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "snippet")
