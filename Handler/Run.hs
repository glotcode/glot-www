module Handler.Run where

import Import
import Util.Handler (maybeApiUser)
import Network.Wai (lazyRequestBody)
import Model.Run.Api (runSnippet)
import Model.Snippet.Api (getSnippet)
import Data.Maybe (fromJust)
import Util (sha1Lazy)

postRunR :: Language -> Handler Value
postRunR lang = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    -- TODO: use shared token for anonymous users
    (runStdout, runStderr, runError) <- liftIO $ runSnippet
        (pack $ show lang) "latest" body $ fromJust (apiUserToken <$> mApiUser)
    mSnippetId <- lookupGetParam "snippet"
    persistRunResult lang mSnippetId (apiUserToken <$> mApiUser)
        (sha1Lazy body) (runStdout, runStderr, runError)
    return $ object [
        "stdout" .= runStdout,
        "stderr" .= runStderr,
        "error" .= runError]


persistRunResult :: Language -> Maybe Text -> Maybe Text -> Text -> (Text, Text, Text) -> Handler ()
persistRunResult lang (Just snippetId) mToken filesHash (runStdout, runStderr, runError)
    | (length runStdout > 0 || length runStderr > 0) && length runError == 0 = do
        snippet <- liftIO $ getSnippet snippetId mToken
        persistRunResult' lang snippetId filesHash
            (snippetFilesHash snippet) (runStdout, runStderr, runError)
persistRunResult _ _ _ _ _ = return ()

persistRunResult' :: Language -> Text -> Text -> Text -> (Text, Text, Text) -> Handler ()
persistRunResult' lang snippetId localHash remoteHash (runStdout, runStderr, runError)
    | localHash == remoteHash = do
        now <- liftIO getCurrentTime
        _ <- runDB $ do
            deleteBy $ UniqueRunResult snippetId
            insertUnique $ RunResult snippetId localHash
                (pack $ show lang) runStdout runStderr runError now
        return ()
persistRunResult' _ _ _ _ _ = return ()
