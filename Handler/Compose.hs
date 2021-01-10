module Handler.Compose where

import Import
import Widget.Editor (editorWidget, footerWidget)
import Widget.Languages (languagesWidget)
import Util.Handler (maybeApiUser, title, titleConcat, urlDecode', apiRequestHeaders)
import Util.Snippet (persistRunParams)
import Util.Alert (successHtml)
import Network.Wai (lazyRequestBody)
import Model.Snippet.Api (addSnippet)
import qualified Text.Blaze as Blaze
import qualified Util.Snippet as Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Database.Persist.Sql as Sql


getComposeLanguagesR :: Handler Html
getComposeLanguagesR = do
    defaultLayout $ do
        setTitle $ title "New snippet - Choose language"
        $(widgetFile "new")

getComposeR :: Language -> Handler Html
getComposeR lang = do
    now <- liftIO getCurrentTime
    let snippet = defaultSnippet lang now
    let files = defaultSnippetFiles lang
    defaultLayout $ do
        setTitle (composeTitle lang)
        setDescription (composeDescription lang)
        $(widgetFile "compose")


composeTitle :: Language -> Blaze.Markup
composeTitle lang =
    if languageIsRunnable lang then
        titleConcat ["Run ", languageName lang, " in the browser"]

    else
        titleConcat ["New ", languageName lang, " snippet"]


composeDescription :: Language -> Text
composeDescription lang =
    if languageIsRunnable lang then
        concat ["Run ", languageName lang, " online in the browser. No installation required."]

    else
        concat ["Create a new ", languageName lang, " snippet"]


postComposeR :: Language -> Handler Value
postComposeR _ = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    snippet <- liftIO $ addSnippet body headers
    persistRunParams (snippetId snippet) stdinData langVersion runCommand
    renderUrl <- getUrlRender
    setMessage $ successHtml "Saved snippet"
    let url = renderUrl $ SnippetR $ snippetId snippet
    return $ object ["url" .= url]


-- TODO: consider creating a new record with non-db fields
defaultSnippet :: Language -> UTCTime -> CodeSnippet
defaultSnippet lang now =
    CodeSnippet
        { codeSnippetSlug = ""
        , codeSnippetLanguage = pack (show lang)
        , codeSnippetTitle = "Untitled"
        , codeSnippetPublic = True
        , codeSnippetUserId = Nothing
        , codeSnippetCreated = now
        , codeSnippetModified = now
        }


defaultSnippetFiles :: Language -> [CodeFile]
defaultSnippetFiles lang =
    pure CodeFile
        { codeFileCodeSnippetId = Sql.toSqlKey 0
        , codeFileName = languageDefaultFname lang
        , codeFileContent = Encoding.encodeUtf8 $ pack $ languageDefaultContent lang
        }
