{-# LANGUAGE DeriveGeneric #-}

module Handler.Run where

import Import hiding (stdin, error)
import qualified Network.Wai as Wai
import qualified GHC.Generics as GHC
import qualified Data.Aeson as Aeson
import qualified Glot.Snippet as Snippet
import qualified Glot.DockerRun as DockerRun
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Settings.Environment as Environment
import qualified Glot.Language as Language
import qualified Util.Handler as Handler


data RunPayload = RunPayload
    { files :: NonEmpty.NonEmpty Snippet.FilePayload
    , stdin :: Maybe Text
    , command :: Maybe Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON RunPayload



postRunR :: Language.Id -> Handler Value
postRunR langId = do
    maybeLanguage <- Handler.lookupLanguage langId
    language <- Handler.fromMaybeOrJsonError maybeLanguage $ Handler.JsonErrorResponse status404 "Language is not supported"
    runConfig <- Handler.fromMaybeOrJsonError (Language.runConfig language) $ Handler.JsonErrorResponse status400 "Language is not runnable"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    dockerRunConfig <- liftIO lookupDockerRunConfig
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            result <- liftIO $ DockerRun.run dockerRunConfig (toRunRequest langId runConfig payload)
            case result of
                Left err -> do
                    print (DockerRun.debugError err)
                    sendResponseStatus status400 (formatRunError err)

                Right runResult ->
                    pure (Aeson.toJSON runResult)


lookupDockerRunConfig :: IO DockerRun.Config
lookupDockerRunConfig = do
    baseUrl <- Environment.dockerRunBaseUrl
    accessToken <- Environment.dockerRunAccessToken
    responseTimeout <- Environment.dockerRunResponseTimeout
    pure DockerRun.Config{..}


formatRunError :: DockerRun.Error -> Value
formatRunError err =
    case err of
        DockerRun.ParseUrlError ->
            Aeson.object ["message" .= (Aeson.String (pack $ DockerRun.formatError err))]

        DockerRun.HttpException _ ->
            Aeson.object ["message" .= (Aeson.String (pack $ DockerRun.formatError err))]

        DockerRun.DecodeSuccessResponseError _ reason ->
            Aeson.object ["message" .= (Aeson.String ("Failed to decode response body from docker-run: " <> pack reason))]

        DockerRun.DecodeErrorResponseError body _ ->
            Aeson.object ["message" .= (Aeson.String (Encoding.decodeUtf8With Encoding.Error.lenientDecode body)) ]

        DockerRun.ApiError DockerRun.ErrorBody{..} ->
            Aeson.object ["message" .= (Aeson.String message)]


toRunRequest :: Language.Id -> Language.RunConfig -> RunPayload -> DockerRun.RunRequest
toRunRequest language Language.RunConfig{..} RunPayload{..} =
    DockerRun.RunRequest
        { image = containerImage
        , payload = DockerRun.RunRequestPayload{..}
        }


runApiToken :: Maybe ApiUser -> Text -> Text
runApiToken (Just user) _ = apiUserToken user
runApiToken _ token = token

--persistRunResult :: Language -> Maybe Text -> [Header] -> Text -> (Text, Text, Text) -> Handler ()
--persistRunResult lang (Just snippetId) headers localHash (runStdout, runStderr, runError)
--    | (length runStdout > 0 || length runStderr > 0) && length runError == 0 = do
--        eSnippet <- liftIO $ safeGetSnippet snippetId headers
--        runParams <- runDB $ getBy $ UniqueRunParams snippetId
--        case eSnippet of
--            Left _ -> return ()
--            Right snippet -> do
--                persistRunResult' lang snippetId localHash
--                    (snippetHash snippet $ formatRunParams runParams) (runStdout, runStderr, runError)
--persistRunResult _ _ _ _ _ = return ()
--
--persistRunResult' :: Language -> Text -> Text -> Text -> (Text, Text, Text) -> Handler ()
--persistRunResult' lang snippetId localHash remoteHash (runStdout, runStderr, runError)
--    | localHash == remoteHash = do
--        now <- liftIO getCurrentTime
--        _ <- runDB $ do
--            deleteBy $ UniqueRunResult snippetId
--            insertUnique $ RunResult snippetId localHash
--                (pack $ show lang) runStdout runStderr runError now
--        return ()
--persistRunResult' _ _ _ _ _ = return ()
--
--safeGetSnippet :: Text -> [Header] -> IO (Either SomeException Snippet)
--safeGetSnippet snippetId headers = try $ getSnippet snippetId headers
