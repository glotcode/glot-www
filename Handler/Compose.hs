module Handler.Compose where

import Import
import Widget
import Network.Wai (lazyRequestBody)
import Model.Snippet.Api (addSnippet)

getComposeR :: Language -> Handler Html
getComposeR lang = do
    let snippet = defaultSnippet lang
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "compose")

postComposeR :: Language -> Handler Value
postComposeR _ = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    snippet <- liftIO $ addSnippet body Nothing
    renderUrl <- getUrlRender
    let url = renderUrl $ SnippetR $ snippetId snippet
    return $ object ["url" .= url]

defaultSnippet :: Language -> Snippet
defaultSnippet lang =
    Snippet{
        snippetId="",
        snippetLanguage="",
        snippetTitle="",
        snippetPublic=True,
        snippetUrl="",
        snippetModified="",
        snippetCreated="",
        snippetFiles=defaultSnippetFiles lang
    }

defaultSnippetFiles :: Language -> [SnippetFile]
defaultSnippetFiles lang =
    [SnippetFile{
        snippetFileName="main.js",
        snippetFileContent=pack $ languageDefaultContent lang
    }]
