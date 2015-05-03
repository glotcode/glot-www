module Handler.Snippet where

import Import
import Widget.Editor (editorWidget)
import Widget.RunResult (runResultWidget)
import Util.Handler (maybeApiUser, title)
import Util.Snippet (isSnippetOwner, ensureLanguageVersion, persistLanguageVersion)
import Util.Alert (successHtml)
import Model.Snippet.Api (getSnippet, updateSnippet)
import Network.Wai (lazyRequestBody)

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
            runResult <- runDB $ getBy $ UniqueRunResultHash
                snippetId $ snippetFilesHash snippet
            let lang = toLanguage $ snippetLanguage snippet
            defaultLayout $ do
                setTitle $ title $ snippetTitle snippet
                $(widgetFile "snippet")

putSnippetR :: Text -> Handler Value
putSnippetR snippetId = do
    langVersion <- ensureLanguageVersion <$> lookupGetParam "version"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    _ <- liftIO $ updateSnippet snippetId body $ apiUserToken <$> mApiUser
    persistLanguageVersion snippetId langVersion
    setMessage $ successHtml "Updated snippet"
    return $ object []
