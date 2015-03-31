module Util.Http (
    httpGet,
    httpPost,
    httpPut
) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.Conduit
import Data.Conduit.Binary (sinkLbs)
import Import.NoFoundation hiding (newManager)
import Data.Text (append)
import qualified Data.ByteString.Lazy as L

httpGet :: String -> Maybe Text -> IO L.ByteString
httpGet url authToken = do
    runResourceT $ do
        req <- parseUrl url
        let req2 = req {
                method = "GET",
                redirectCount = 0,
                requestHeaders = headers authToken}
        manager <- liftIO $ newManager conduitManagerSettings
        res2 <- http req2 manager
        responseBody res2 $$+- sinkLbs

httpPost :: String -> Maybe Text -> L.ByteString -> IO L.ByteString
httpPost = httpRequest "POST"

httpPut :: String -> Maybe Text -> L.ByteString -> IO L.ByteString
httpPut = httpRequest "PUT"

httpRequest :: Method -> String -> Maybe Text -> L.ByteString -> IO L.ByteString
httpRequest method url authToken payload = do
    runResourceT $ do
        req <- parseUrl url
        let req2 = req {
                method = method,
                requestBody = RequestBodyLBS payload,
                redirectCount = 0,
                requestHeaders = headers authToken}
        manager <- liftIO $ newManager conduitManagerSettings
        res2 <- http req2 manager
        responseBody res2 $$+- sinkLbs

headers :: Maybe Text -> RequestHeaders
headers Nothing = [(hContentType, "application/json")]
headers (Just authToken) = [
        (hContentType, "application/json"),
        (hAuthorization, authHeader authToken)
    ]

authHeader :: Text -> ByteString
authHeader = encodeUtf8 . append "Token "
