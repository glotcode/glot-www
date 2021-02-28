{-# LANGUAGE DeriveGeneric #-}

module Handler.Run where

import Import
import Util.Handler (maybeApiUser, apiRequestHeaders)
import Model.Run.Api (runSnippet)
import Settings.Environment (runApiAnonymousToken)
import qualified Network.Wai as Wai
import qualified GHC.Generics as GHC
import qualified Data.Aeson as Aeson
import qualified Glot.Snippet as Snippet
import qualified Glot.DockerRun as DockerRun
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import Data.Function ((&))


data RunPayload = RunPayload
    { files :: NonEmpty.NonEmpty Snippet.FilePayload
    , stdin :: Maybe Text
    , command :: Maybe Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON RunPayload


-- TODO: get docker image
postRunR :: Language -> Handler Value
postRunR lang = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    -- persist <- fromMaybe "true" <$> lookupGetParam "persist"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    mUserId <- maybeAuthId
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            result <- liftIO $ DockerRun.run (toRunRequest lang "glot/bash:latest" payload)
            case result of
                Left err -> do
                    print err
                    sendResponseStatus status400 (formatRunError err)

                Right runResult ->
                    pure (Aeson.toJSON runResult)


formatRunError :: DockerRun.Error -> Value
formatRunError err =
    case err of
        DockerRun.HttpException _ ->
            Aeson.object ["message" .= (Aeson.String (pack $ DockerRun.formatError err))]

        DockerRun.DecodeSuccessResponseError _ reason ->
            Aeson.object ["message" .= (Aeson.String ("Failed to decode response body from docker-run: " <> pack reason))]

        DockerRun.DecodeErrorResponseError body _ ->
            Aeson.object ["message" .= (Aeson.String (Encoding.decodeUtf8With Encoding.Error.lenientDecode body)) ]

        DockerRun.ApiError DockerRun.ErrorBody{..} ->
            Aeson.object ["message" .= (Aeson.String message)]


toRunRequest :: Language -> Text -> RunPayload -> DockerRun.RunRequest
toRunRequest language dockerImage RunPayload{..} =
    DockerRun.RunRequest
        { image = dockerImage
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
