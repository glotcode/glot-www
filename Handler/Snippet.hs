module Handler.Snippet where

import Import
import Widget.Editor (editorWidget)
import Widget.RunResult (runResultWidget)
import Util.Handler (maybeApiUser)
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
                $(combineScripts 'StaticR [lib_ace_ace_js])
                setTitle $ "glot.io"
                $(widgetFile "snippet")

putSnippetR :: Text -> Handler Value
putSnippetR snippetId = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    _ <- liftIO $ updateSnippet snippetId body $ apiUserToken <$> mApiUser
    return $ object []

isSnippetOwner :: Maybe ApiUser -> Snippet -> Bool
isSnippetOwner Nothing _ = False
isSnippetOwner (Just apiUser) snippet =
    apiUserSnippetsId apiUser == snippetOwner snippet
