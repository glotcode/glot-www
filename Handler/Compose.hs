module Handler.Compose where

import Import
import Widget.Editor (editorWidget)
import Widget.RunResult (runResultWidget)
import Util.Handler (maybeApiUser)
import Network.Wai (lazyRequestBody)
import Model.Snippet.Api (addSnippet)

getComposeR :: Language -> Handler Html
getComposeR lang = do
    auth <- maybeAuth
    let snippet = defaultSnippet lang
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "compose")

postComposeR :: Language -> Handler Value
postComposeR _ = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    snippet <- liftIO $ addSnippet body $ apiUserToken <$> mApiUser
    renderUrl <- getUrlRender
    let url = renderUrl $ SnippetR $ snippetId snippet
    return $ object ["url" .= url]

defaultSnippet :: Language -> Snippet
defaultSnippet lang =
    Snippet{
        snippetId="",
        snippetLanguage=pack $ show lang,
        snippetTitle="Untitled",
        snippetPublic=True,
        snippetOwner="",
        snippetFilesHash="",
        snippetModified="",
        snippetCreated="",
        snippetFiles=defaultSnippetFiles lang
    }

defaultSnippetFiles :: Language -> [SnippetFile]
defaultSnippetFiles lang =
    [SnippetFile{
        snippetFileName=languageDefaultFname lang,
        snippetFileContent=pack $ languageDefaultContent lang
    }]
