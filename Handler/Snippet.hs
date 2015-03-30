module Handler.Snippet where

import Import
import Widget
import Model.Snippet.Api (getSnippet)

getSnippetR :: Text -> Handler Html
getSnippetR snippetId = do
    snippet <- liftIO $ getSnippet snippetId Nothing
    let lang = toLanguage $ snippetLanguage snippet
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "snippet")
