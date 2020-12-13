module Handler.Snippet where

import Import hiding (pack)
import Widget.Editor (editorWidget, footerWidget)
import Widget.RunResult (runResultWidget)
import Widget.Share (shareWidget)
import Util.Handler (maybeApiUser, titleConcat, urlDecode', apiRequestHeaders)
import Util.Snippet (isSnippetOwner, persistRunParams, metaDescription, formatRunParams)
import Util.Alert (successHtml)
import Model.Snippet.Api (getSnippet, updateSnippet, deleteSnippet)
import Network.Wai (lazyRequestBody)
import Text.Hamlet (hamletFile, shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (pack)


getSnippetR :: Text -> Handler Html
getSnippetR snippetId = do
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    req <- reqWaiRequest <$> getRequest
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    eSnippet <- liftIO $ try $ getSnippet snippetId headers
    case eSnippet of
        Left err@(HttpExceptionRequest _ (StatusCodeException response _)) ->
            if statusCode (responseStatus response) == 404 then
                notFound

            else
                throwIO err

        Left err ->
            throwIO err

        Right snippet -> do
            (profile, runParams, runResult) <- runDB $ do
                p <- getBy $ UniqueSnippetsApiId $ snippetOwner snippet
                params <- getBy $ UniqueRunParams snippetId
                res <- getBy $ UniqueRunResultHash snippetId $ (snippetHash snippet $ formatRunParams params)
                return (p, params, res)
            let lang = toLanguage $ snippetLanguage snippet
            defaultLayout $ do
                setTitle $ titleConcat [snippetTitle snippet, " - ", languageName lang, " Snippet"]
                toWidgetHead $(hamletFile "templates/snippet/opengraph.hamlet")
                toWidgetHead $(hamletFile "templates/snippet/twitter-card.hamlet")
                $(widgetFile "snippet")

putSnippetR :: Text -> Handler Value
putSnippetR snippetId = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    _ <- liftIO $ updateSnippet snippetId body headers
    persistRunParams snippetId stdinData langVersion runCommand
    setMessage $ successHtml "Updated snippet"
    return $ object []

deleteSnippetR :: Text -> Handler Value
deleteSnippetR snippetId = do
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    req <- reqWaiRequest <$> getRequest
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    _ <- liftIO $ deleteSnippet snippetId headers
    return $ object []

getSnippetEmbedR :: Text -> Handler Html
getSnippetEmbedR snippetId = do
    req <- reqWaiRequest <$> getRequest
    let headers = apiRequestHeaders req Nothing
    eSnippet <- liftIO $ try $ getSnippet snippetId headers
    case eSnippet of
        Left err@(HttpExceptionRequest _ (StatusCodeException response _)) ->
            if statusCode (responseStatus response) == 404 then
                notFound

            else
                throwIO err

        Left err ->
            throwIO err

        Right snippet -> do
            (profile, runParams) <- runDB $ do
                p <- getBy $ UniqueSnippetsApiId $ snippetOwner snippet
                params <- getBy $ UniqueRunParams snippetId
                return (p, params)
            let lang = toLanguage $ snippetLanguage snippet
            defaultLayout $ do
                setTitle $ titleConcat [snippetTitle snippet, " - ", languageName lang, " Snippet"]
                $(widgetFile "snippet/embed")

getSnippetRawR :: Text -> Handler Html
getSnippetRawR snippetId = do
    req <- reqWaiRequest <$> getRequest
    let headers = apiRequestHeaders req Nothing
    eSnippet <- liftIO $ try $ getSnippet snippetId headers
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
                    redirect $ SnippetRawFileR snippetId $ snippetFileName f
                _ -> do
                    let lang = toLanguage $ snippetLanguage snippet
                    defaultLayout $ do
                        setTitle $ titleConcat [snippetTitle snippet, " - ", languageName lang, " Snippet"]
                        $(widgetFile "snippet/raw")

getSnippetRawFileR :: Text -> Text -> Handler Text
getSnippetRawFileR snippetId filename = do
    req <- reqWaiRequest <$> getRequest
    let headers = apiRequestHeaders req Nothing
    eSnippet <- liftIO $ try $ getSnippet snippetId headers
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
