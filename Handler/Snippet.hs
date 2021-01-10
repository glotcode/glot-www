module Handler.Snippet where

import Import hiding (pack)
import Widget.Editor (editorWidget, footerWidget)
import Widget.RunResult (runResultWidget)
import Widget.Share (shareWidget)
import Util.Handler (maybeApiUser, titleConcat, urlDecode', apiRequestHeaders)
import Util.Alert (successHtml)
import Model.Snippet.Api (getSnippet, updateSnippet, deleteSnippet)
import Network.Wai (lazyRequestBody)
import Text.Hamlet (hamletFile, shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (pack)
import qualified Util.Snippet as Snippet


getSnippetR :: Text -> Handler Html
getSnippetR slug = do
    mUserId <- maybeAuthId
    (snippet, files, profile, runParams, runResult) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        -- TODO: fix
        runResult <- pure Nothing -- getBy $ UniqueRunResultHash slug $ (snippetHash snippet $ formatRunParams runParams)
        pure (snippet, map entityVal files, profile, runParams, runResult)
    let lang = toLanguage $ codeSnippetLanguage snippet
    let userIsSnippetOwner = mUserId == codeSnippetUserId snippet
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", languageName lang, " Snippet"]
        setDescription (snippetDescription lang)
        toWidgetHead $(hamletFile "templates/snippet/opengraph.hamlet")
        toWidgetHead $(hamletFile "templates/snippet/twitter-card.hamlet")
        $(widgetFile "snippet")


snippetDescription :: Language -> Text
snippetDescription lang =
    if languageIsRunnable lang then
        concat ["Run this ", languageName lang, " code snippet in the browser."]

    else
        concat [languageName lang, " snippet"]


putSnippetR :: Text -> Handler Value
putSnippetR slug = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    _ <- liftIO $ updateSnippet slug body headers
    Snippet.persistRunParams slug stdinData langVersion runCommand
    setMessage $ successHtml "Updated snippet"
    return $ object []

deleteSnippetR :: Text -> Handler Value
deleteSnippetR slug = do
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    req <- reqWaiRequest <$> getRequest
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    _ <- liftIO $ deleteSnippet slug headers
    return $ object []

getSnippetEmbedR :: Text -> Handler Html
getSnippetEmbedR slug = do
    (snippet, files, profile, runParams) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        pure (snippet, map entityVal files, profile, runParams)
    let lang = toLanguage $ codeSnippetLanguage snippet
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", languageName lang, " Snippet"]
        $(widgetFile "snippet/embed")

getSnippetRawR :: Text -> Handler Html
getSnippetRawR slug = do
    req <- reqWaiRequest <$> getRequest
    let headers = apiRequestHeaders req Nothing
    eSnippet <- liftIO $ try $ getSnippet slug headers
    case eSnippet of
        Left err@(HttpExceptionRequest _ (StatusCodeException response _)) ->
            if statusCode (responseStatus response) == 404 then
                notFound

            else
                throwIO err

        Left err ->
            throwIO err

        Right snippet ->
            case snippetFiles snippet of
                [f] ->
                    redirect $ SnippetRawFileR slug $ snippetFileName f
                _ -> do
                    let lang = toLanguage $ snippetLanguage snippet
                    defaultLayout $ do
                        setTitle $ titleConcat [snippetTitle snippet, " - ", languageName lang, " Snippet"]
                        $(widgetFile "snippet/raw")

getSnippetRawFileR :: Text -> Text -> Handler Text
getSnippetRawFileR slug filename = do
    req <- reqWaiRequest <$> getRequest
    let headers = apiRequestHeaders req Nothing
    eSnippet <- liftIO $ try $ getSnippet slug headers
    case eSnippet of
        Left err@(HttpExceptionRequest _ (StatusCodeException response _)) ->
            if statusCode (responseStatus response) == 404 then
                notFound

            else
                throwIO err

        Left e ->
            throwIO e

        Right snippet -> do
            return $ pack $ renderHtml $(shamletFile "templates/snippet/raw/file.hamlet")

getFileContent :: Snippet -> Text -> Maybe Text
getFileContent snippet name =
    snippetFileContent <$> (listToMaybe $ filter (\f -> snippetFileName f == name) (snippetFiles snippet))
