module Handler.Run where

import Import
import Util.Handler (maybeApiUser)
import Network.Wai (lazyRequestBody)
import Model.Run.Api (runSnippet)
import Data.Maybe (fromJust)

postRunR :: Language -> Handler Value
postRunR lang = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    -- TODO: use shared token for anonymous users
    (runStdout, runStderr, runError) <- liftIO $ runSnippet
        (pack $ show lang) "latest" body $ fromJust (apiUserToken <$> mApiUser)
    return $ object [
        "stdout" .= runStdout,
        "stderr" .= runStderr,
        "error" .= runError]
