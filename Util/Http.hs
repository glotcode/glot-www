module Util.Http (
    Links(..),
    httpGet,
    httpGetLink,
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

import qualified Network.Wreq as W
import Control.Lens

data Links = Links {
    relNext :: Maybe Text,
    relPrev :: Maybe Text,
    relFirst :: Maybe Text,
    relLast :: Maybe Text
} deriving (Show)

httpGetLink :: String -> Maybe Text -> IO (L.ByteString, Links)
httpGetLink url authToken = do
    r <- W.getWith (reqOptions authToken) url
    return $ (r ^. W.responseBody, toLinks r)

toLinks :: Response body -> Links
toLinks r =
    Links{
        relNext=decodeUtf8 <$> (r ^? W.responseLink "rel" "next" . W.linkURL),
        relPrev=decodeUtf8 <$> (r ^? W.responseLink "rel" "prev" . W.linkURL),
        relFirst=decodeUtf8 <$> (r ^? W.responseLink "rel" "first" . W.linkURL),
        relLast=decodeUtf8 <$> (r ^? W.responseLink "rel" "last" . W.linkURL)
    }

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

reqOptions :: Maybe Text -> W.Options
reqOptions Nothing = W.defaults
reqOptions (Just authToken) =
    W.defaults & W.header "Authorization" .~ [authHeader authToken]

headers :: Maybe Text -> RequestHeaders
headers Nothing = [(hContentType, "application/json")]
headers (Just authToken) = [
        (hContentType, "application/json"),
        (hAuthorization, authHeader authToken)
    ]

authHeader :: Text -> ByteString
authHeader = encodeUtf8 . append "Token "
