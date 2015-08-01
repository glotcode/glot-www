module Handler.Compose where

import Import
import Widget.Editor (editorWidget)
import Widget.RunResult (runResultWidget)
import Util.Handler (maybeApiUser, title, titleConcat, urlDecode')
import Util.Snippet (persistLanguageVersion, persistRunCommand)
import Util.Alert (successHtml)
import Network.Wai (lazyRequestBody)
import Model.Snippet.Api (addSnippet)

getComposeLanguagesR :: Handler Html
getComposeLanguagesR = do
    defaultLayout $ do
        setTitle $ title "Languages"
        $(widgetFile "languages")

getComposeR :: Language -> Handler Html
getComposeR lang = do
    let snippet = defaultSnippet lang
    defaultLayout $ do
        setTitle $ titleConcat ["New ", languageName lang, " Snippet"]
        $(widgetFile "compose")

postComposeR :: Language -> Handler Value
postComposeR _ = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    snippet <- liftIO $ addSnippet body $ apiUserToken <$> mApiUser
    persistLanguageVersion (snippetId snippet) langVersion
    persistRunCommand (snippetId snippet) runCommand
    renderUrl <- getUrlRender
    setMessage $ successHtml "Saved snippet"
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
