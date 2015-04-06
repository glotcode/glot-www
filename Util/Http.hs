module Util.Http (
    Links(..),
    httpGet,
    httpGetLink,
    httpPost,
    httpPut
) where

import Import.NoFoundation hiding (responseBody)
import Data.Text (append)
import qualified Data.ByteString.Lazy as L
import Network.Wreq
import Control.Lens

data Links = Links {
    relNext :: Maybe Text,
    relPrev :: Maybe Text,
    relFirst :: Maybe Text,
    relLast :: Maybe Text
} deriving (Show)

toLinks :: Response body -> Links
toLinks r =
    Links{
        relNext=decodeUtf8 <$> (r ^? responseLink "rel" "next" . linkURL),
        relPrev=decodeUtf8 <$> (r ^? responseLink "rel" "prev" . linkURL),
        relFirst=decodeUtf8 <$> (r ^? responseLink "rel" "first" . linkURL),
        relLast=decodeUtf8 <$> (r ^? responseLink "rel" "last" . linkURL)
    }

httpGet :: String -> Maybe Text -> IO L.ByteString
httpGet url authToken = do
    r <- getWith (reqOptions authToken) url
    return $ r ^. responseBody

httpGetLink :: String -> Maybe Text -> IO (L.ByteString, Links)
httpGetLink url authToken = do
    r <- getWith (reqOptions authToken) url
    return $ (r ^. responseBody, toLinks r)

httpPost :: String -> Maybe Text -> L.ByteString -> IO L.ByteString
httpPost url authToken payload = do
    r <- postWith (reqOptions authToken) url payload
    return $ r ^. responseBody

httpPut :: String -> Maybe Text -> L.ByteString -> IO L.ByteString
httpPut url authToken payload = do
    r <- putWith (reqOptions authToken) url payload
    return $ r ^. responseBody

reqOptions :: Maybe Text -> Options
reqOptions Nothing = defaults & header "Content-type" .~ ["application/json"]
reqOptions (Just authToken) =
    defaults &
        header "Authorization" .~ [authHeader authToken] &
        header "Content-type" .~ ["application/json"]

authHeader :: Text -> ByteString
authHeader = encodeUtf8 . append "Token "
