module Handler.Run where

import Import
import Util.Handler (maybeApiUser, apiRequestHeaders)
import Network.Wai (lazyRequestBody)
import Model.Run.Api (runSnippet)
import Model.Snippet.Api (getSnippet)
import Util.Snippet (formatRunParams)
import Settings.Environment (runApiAnonymousToken)

postRunR :: Language -> Handler Value
postRunR lang = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    runAnonToken <- liftIO runApiAnonymousToken
    let authToken = runApiToken mApiUser runAnonToken
    let headers = apiRequestHeaders req $ Just authToken
    res <- liftIO $ runSnippet (pack $ show lang) langVersion body headers
    case res of
        Left errorMsg ->
            sendResponseStatus status400 $ object ["message" .= errorMsg]
        Right (runStdout, runStderr, runError) -> do
            mSnippetId <- lookupGetParam "snippet"
            let userToken = apiUserToken <$> mApiUser
            let persistHeaders = apiRequestHeaders req userToken
            let localHash = snippetHashJson body langVersion
            persistRunResult lang mSnippetId persistHeaders localHash (runStdout, runStderr, runError)
            return $ object [
                "stdout" .= runStdout,
                "stderr" .= runStderr,
                "error" .= runError]

runApiToken :: Maybe ApiUser -> Text -> Text
runApiToken (Just user) _ = apiUserToken user
runApiToken _ token = token

persistRunResult :: Language -> Maybe Text -> [Header] -> Text -> (Text, Text, Text) -> Handler ()
persistRunResult lang (Just snippetId) headers localHash (runStdout, runStderr, runError)
    | (length runStdout > 0 || length runStderr > 0) && length runError == 0 = do
        eSnippet <- liftIO $ safeGetSnippet snippetId headers
        runParams <- runDB $ getBy $ UniqueRunParams snippetId
        case eSnippet of
            Left _ -> return ()
            Right snippet -> do
                persistRunResult' lang snippetId localHash
                    (snippetHash snippet $ formatRunParams runParams) (runStdout, runStderr, runError)
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

safeGetSnippet :: Text -> [Header] -> IO (Either SomeException Snippet)
safeGetSnippet snippetId headers = try $ getSnippet snippetId headers
