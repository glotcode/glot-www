module Handler.Snippets where

import Import
import Model.Snippet.Api (listSnippets)

getSnippetsR :: Handler Html
getSnippetsR = do
    snippets <- liftIO $ listSnippets Nothing
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "snippets")
