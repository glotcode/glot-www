module Handler.Snippet where

import Import hiding (pack)
import Widget.Editor (editorWidget)
import Widget.RunResult (runResultWidget)
import Widget.Share (shareWidget)
import Util.Handler (maybeApiUser, titleConcat, urlDecode')
import Util.Snippet (isSnippetOwner, persistLanguageVersion, persistRunCommand, metaDescription)
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
    eSnippet <- liftIO $ try (getSnippet snippetId $ apiUserToken <$> mApiUser)
    case eSnippet of
        Left (StatusCodeException s _ _)
            | statusCode s == 404 -> notFound
        Left e -> throwIO e
        Right snippet -> do
            runResult <- runDB $ getBy $ UniqueRunResultHash snippetId $ snippetContentHash snippet
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
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    _ <- liftIO $ updateSnippet snippetId body $ apiUserToken <$> mApiUser
    persistLanguageVersion snippetId langVersion
    persistRunCommand snippetId runCommand
    setMessage $ successHtml "Updated snippet"
    return $ object []

deleteSnippetR :: Text -> Handler Value
deleteSnippetR snippetId = do
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    _ <- liftIO $ deleteSnippet snippetId $ apiUserToken <$> mApiUser
    return $ object []

getSnippetEmbedR :: Text -> Handler Html
getSnippetEmbedR snippetId = do
    eSnippet <- liftIO $ try (getSnippet snippetId Nothing)
    case eSnippet of
        Left (StatusCodeException s _ _)
            | statusCode s == 404 -> notFound
        Left e -> throwIO e
        Right snippet -> do
            let runResult = Nothing
            let lang = toLanguage $ snippetLanguage snippet
            defaultLayout $ do
                setTitle $ titleConcat [snippetTitle snippet, " - ", languageName lang, " Snippet"]
                $(widgetFile "snippet/embed")

getSnippetRawR :: Text -> Handler Html
getSnippetRawR snippetId = do
    eSnippet <- liftIO $ try (getSnippet snippetId Nothing)
    case eSnippet of
        Left (StatusCodeException s _ _)
            | statusCode s == 404 -> notFound
        Left e -> throwIO e
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
    eSnippet <- liftIO $ try (getSnippet snippetId Nothing)
    case eSnippet of
        Left (StatusCodeException s _ _)
            | statusCode s == 404 -> notFound
        Left e -> throwIO e
        Right snippet -> do
            return $ pack $ renderHtml $(shamletFile "templates/snippet/raw/file.hamlet")

getFileContent :: Snippet -> Text -> Maybe Text
getFileContent snippet name =
    snippetFileContent <$> (listToMaybe $ filter (\f -> snippetFileName f == name) (snippetFiles snippet))
