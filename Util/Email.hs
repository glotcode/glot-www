module Util.Email (
    sendEmail,
) where

import Prelude
import System.Environment (getEnv)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object, encode)
import Data.Conduit.List (sinkNull)
import Data.Text.Lazy (Text)


serverUrl :: IO String
serverUrl = getEnv "EMAIL_SERVER_URL"

sendEmail :: Text -> IO ()
sendEmail msg = do
    let payload = encode $ object ["message" .= msg]
    url <- serverUrl
    _ <- runResourceT $ do
        req <- parseUrl url
        let req2 = req {
                method = "POST",
                requestBody = RequestBodyLBS payload,
                redirectCount = 0,
                requestHeaders = [(hContentType, "application/json")]}
        manager <- liftIO $ newManager conduitManagerSettings
        res2 <- http req2 manager
        responseBody res2 $$+- sinkNull
    return ()
